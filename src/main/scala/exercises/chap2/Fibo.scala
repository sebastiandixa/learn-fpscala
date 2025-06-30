object FiboProgram:

    def fib(n: Int): Int =
        @annotation.tailrec
        def fibonacciLoop(n: Int, lastVal: Int, previousToLastVal: Int): Int =
            if n == 0 then lastVal
            else if n == 1 then previousToLastVal
            else fibonacciLoop(n - 1, previousToLastVal, lastVal + previousToLastVal)
    
        fibonacciLoop(n, 0, 1)


    @main def runFibonacci(): Unit =
        println(fib(0))
        println(fib(1))
        println(fib(2))
        println(fib(8))