#!/usr/bin/env node
// Waybar custom module: focused niri window title, cross-faded on change.
// Each transition emits two outputs: old-text with a `fade-out-*` class, then
// (after FADE_MS) new-text with a `fade-in-*` class. The `-a`/`-b` suffix
// alternates on every emit so GTK always sees a class change and restarts the
// keyframe animation, even when fade-in follows fade-out of the same kind.

const { spawn } = require("child_process");
const readline = require("readline");

const FADE_MS = 150;

const proc = spawn("niri", ["msg", "--json", "event-stream"], {
  stdio: ["ignore", "pipe", "ignore"],
});
const rl = readline.createInterface({ input: proc.stdout });

const windows = new Map();
let focusedId = null;
let lastTitle = null;
let toggle = false;
let pendingTimer = null;

function emit(text, kind) {
  toggle = !toggle;
  process.stdout.write(
    JSON.stringify({
      text,
      class: `${kind}-${toggle ? "a" : "b"}`,
    }) + "\n",
  );
}

function resolveFocus() {
  if (focusedId != null && windows.has(focusedId)) {
    const w = windows.get(focusedId);
    return { title: w.title || "", appId: w.app_id || "" };
  }
  return { title: "", appId: "" };
}

function maybeTransition() {
  const { title } = resolveFocus();
  if (title === lastTitle) return;
  const oldTitle = lastTitle ?? "";
  lastTitle = title;
  if (pendingTimer) clearTimeout(pendingTimer);
  if (oldTitle === "") {
    emit(title, "fade-in");
    return;
  }
  emit(oldTitle, "fade-out");
  pendingTimer = setTimeout(() => {
    emit(title, "fade-in");
    pendingTimer = null;
  }, FADE_MS);
}

rl.on("line", (line) => {
  let event;
  try {
    event = JSON.parse(line);
  } catch {
    return;
  }
  if ("WindowsChanged" in event) {
    windows.clear();
    for (const w of event.WindowsChanged.windows) {
      windows.set(w.id, w);
      if (w.is_focused) focusedId = w.id;
    }
  } else if ("WindowOpenedOrChanged" in event) {
    const w = event.WindowOpenedOrChanged.window;
    windows.set(w.id, w);
    if (w.is_focused) focusedId = w.id;
  } else if ("WindowClosed" in event) {
    windows.delete(event.WindowClosed.id);
  } else if ("WindowFocusChanged" in event) {
    focusedId = event.WindowFocusChanged.id;
  }
  maybeTransition();
});

rl.on("close", () => process.exit(0));
