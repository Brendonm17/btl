import time

def fib(n):
    if n < 2: return n
    return fib(n - 1) + fib(n - 2)

start = time.time()
result = fib(35)
end = time.time()

print("Result:")
print(result)
print("Python Time (seconds):")
print(end - start)