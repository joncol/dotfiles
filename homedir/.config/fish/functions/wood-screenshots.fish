# Defined in /tmp/fish.7LrVAX/wood-screenshots.fish @ line 2
function wood-screenshots
    xclip -o | xargs -I% sh -c '{ echo Getting Wood screenshots for correlation ID: %...; aws s3 sync --exclude "*" --include "*%*" s3://ebway/artifacts/production . ; }'
end
