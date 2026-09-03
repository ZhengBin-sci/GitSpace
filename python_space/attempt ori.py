import sys
import time

message = "✨ 既然没啥想法，那就祝你今天代码一次过，Bug 全退散！✨"

for char in message:
    sys.stdout.write(char)
    sys.stdout.flush()
    time.sleep(0.1)  # 打字机效果，稍微停顿一下

print("\n")  # 最后空一行，保持排版美观