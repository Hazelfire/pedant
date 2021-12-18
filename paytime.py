import datetime
import sys

now = datetime.datetime.now()
finish = datetime.datetime(year=2021, month=12, day=25)

needed_time = 200 / 3
current_time = int(sys.argv[1])
time_left = needed_time - current_time
print(time_left / ((finish - now).total_seconds() / 60 / 60 / 24))
