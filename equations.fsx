open System

// Уравнение 4: 3x - 14 + e^x - e^-x = 0
let equation4 (x: float) = 3.0 * x - 14.0 + Math.Exp(x) - Math.Exp(-x)
let derivative4 (x: float) = 3.0 + Math.Exp(x) + Math.Exp(-x) // Производная

// Уравнение 5: sqrt(1 - x) - tg(x) = 0
let equation5 (x: float) = Math.Sqrt(1.0 - x) - Math.Tan(x)
let iterationFunction5 (x: float) = Math.Sqrt(1.0 - x) // Вспомогательная функция для метода итераций
let derivative5 (x: float) = -1.0 / (2.0 * Math.Sqrt(1.0 - x)) - 1.0 / Math.Pow(Math.Cos(x), 2.0) // Производная

// Уравнение 6: x + cos(x^0.52 + 2) = 0
let equation6 (x: float) = x + Math.Cos(Math.Pow(x, 0.52) + 2.0)
let derivative6 (x: float) = 1.0 - 0.52 * Math.Sin(Math.Pow(x, 0.52) + 2.0) * Math.Pow(x, -0.48) // Производная

// Метод дихотомии
let rec bisectionMethod (f: float -> float) a b eps =
    let c = (a + b) / 2.0
    if Math.Abs(f c) < eps then c
    elif f a * f c < 0.0 then bisectionMethod f a c eps
    else bisectionMethod f c b eps

// Метод итераций
let rec iterationMethod (f: float -> float) (g: float -> float) x0 eps =
    let x1 = g x0
    if Math.Abs(x1 - x0) < eps then x1
    else iterationMethod f g x1 eps

// Метод Ньютона
let rec newtonMethod (f: float -> float) (f': float -> float) x0 eps =
    let x1 = x0 - (f x0 / f' x0)
    if Math.Abs(x1 - x0) < eps then x1
    else newtonMethod f f' x1 eps

// Функция для нахождения корней всех уравнений и методов
let solveEquations () =
    let eps = 1e-6

    printfn "%-15s %-15s %-15s %-15s" "Equation" "Bisection" "Iteration" "Newton"

    // Уравнение 4
    let rootBisection4 = bisectionMethod equation4 1.0 3.0 eps
    let rootIteration4 = iterationMethod equation4 (fun x -> x) 2.0 eps // Фиктивная функция для итераций
    let rootNewton4 = newtonMethod equation4 derivative4 2.0 eps
    printfn "%-15s %-15f %-15f %-15f" "Equation 4" rootBisection4 rootIteration4 rootNewton4

    // Уравнение 5
    let rootBisection5 = bisectionMethod equation5 0.0 1.0 eps
    let rootIteration5 = iterationMethod equation5 iterationFunction5 0.5 eps
    let rootNewton5 = newtonMethod equation5 derivative5 0.5 eps
    printfn "%-15s %-15f %-15f %-15f" "Equation 5" rootBisection5 rootIteration5 rootNewton5

    // Уравнение 6
    let rootBisection6 = bisectionMethod equation6 0.5 1.0 eps
    let rootIteration6 = iterationMethod equation6 (fun x -> x) 0.75 eps // Фиктивная функция для итераций
    let rootNewton6 = newtonMethod equation6 derivative6 0.75 eps
    printfn "%-15s %-15f %-15f %-15f" "Equation 6" rootBisection6 rootIteration6 rootNewton6

// Запуск решения
solveEquations ()
