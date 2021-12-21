import random
import time

start_time = time.time()

randomlist = []
for i in range (0, 500000):
  n = random.randint(0,9)
  randomlist.append(n)

for i in range (0, 500000):
  randomlist[i] = randomlist[i] + 1;

print(sum(randomlist))

print("--- %s seconds ---" % (time.time() - start_time))