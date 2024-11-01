function fibonacci(n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

const n = 20;
console.log(`Fibonacci for n=${20}`);

const promise = new Promise((resolve, reject) => {
    const result = fibonacci(n);
    resolve(result);
});

promise.then(result => {
    console.log(`fibo(${n})=${result}`)
});

console.log(`Main...`)