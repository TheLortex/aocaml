import random, sys
from tqdm import tqdm

n_window        = 500
n_tokens        = 10000
err_position    = 9500

select_window   = 10

tokens = list(range(1,n_window+1)) 
random.shuffle(tokens)
tokens = tokens[:n_window]

numbers= set(tokens)

for i in tqdm(range(n_window, n_tokens)):
    window = tokens[-n_window:]

    if i != err_position:
        p1, p2 = 0, 0
        select_window = select_window // 2
        while p1 == p2 or ((p1 + p2) in numbers):
            p1, p2 = random.choices(sorted(window)[:select_window], k=2) 
            select_window = select_window * 2
        numbers.add(p1 + p2)
        tokens.append(p1 + p2)
    else:
        min_value, max_value = min(window)*2, max(window)*2
        sums = set()
        for x in window:
            for y in window:
                if x != y:
                    sums.add(x+y)
        
        interval_begin = 0
        interval_end = 0
        interval_sum = tokens[0]

        while interval_sum <= min_value or interval_sum >= max_value or interval_sum in sums:
            if interval_sum <= min_value:
                interval_end += 1
                interval_sum += tokens[interval_end]
            else:
                interval_sum -= tokens[interval_begin]
                interval_begin += 1
        
        numbers.add(interval_sum)
        tokens.append(interval_sum)

print("\n".join(str(x) for x in tokens))
print("PART 1:", interval_sum, file=sys.stderr)
interval = tokens[interval_begin:interval_end+1]
print("PART 2:", max(interval) + min(interval), file=sys.stderr)
if interval_sum.bit_length() > 64:
    print("Warning: numbers greater than 2^64 have been generated:", interval_sum.bit_length(), file=sys.stderr)