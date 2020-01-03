# Defined in /tmp/fish.iyKXwc/wood_screenshot.fish @ line 1
function wood_screenshot
	aws s3 sync --exclude "*" --include "*$argv[1]*" s3://ebway/artifacts/production ~/wood_screenshots ;and open ~/wood_screenshots
end
