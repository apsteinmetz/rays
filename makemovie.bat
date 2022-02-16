REM Run this command in the command line using ffmpeg to stitch together a video
REM  using frame file name instead of "frame"
ffmpeg -framerate 60 -i "gwl%03d.png" -vcodec libx264 raymovie.mp4

REM And run this command to convert the video to post to the web:
#ffmpeg -i raymovie.mp4 -pix_fmt yuv420p -profile:v baseline -level 3 -vf scale=-2:-2 rayweb.mp4

