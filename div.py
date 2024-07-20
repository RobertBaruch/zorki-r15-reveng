s1 = 0
s2 = 0
s3 = 0

def run():
    global s1, s2, s3
    s3 = s2
    s2 = 0
    x = 17

    print(f"Begin, x={x:02d}: s1={s1:016b}, s2={s2:016b}, s3={s3:016b}")
    while True:
        c = 0
        if s2 - s1 >= 0:
            c = 1
            s2 = s2 - s1
        # Get the next hi bit from the dividend
        s3hi = s3 >> 15
        # Update quotient
        s3 = ((s3 << 1) & 0xFFFF) | c
        # Update scratch space
        s2 = ((s2 << 1) & 0xFFFF) | s3hi
        x = x - 1
        print(f"Loop,  x={x:02d}: s1={s1:016b}, s2={s2:016b}, s3={s3:016b}")
        if x == 0:
            break
    print(f"End,   x={x:02d}: s1={s1:016b}, s2={s2:016b}, s3={s3:016b}")
    print("After adjustment shift and remainder storage:")
    s1 = s2 >> 1
    s2 = s3
    print(f"End,   x={x:02d}: s1={s1:016b}, s2={s2:016b}")

if __name__ == "__main__":
    s2 = 37
    s1 = 10
    run()