## Description
A trivial example of the [smart constructor pattern](https://wiki.haskell.org/Smart_constructors) in F#. In this example there is a module called `Range` that contains logic on how to test if a value is between two bounds. To enforce the lower bound to always be less than or equal to the upper bound, the module does not expose the constructor of `Range`. Therefore to be able to create a `Range` type, you need to use `Range.make`.
## Motivation
I was curious if it was possible to port the following haskell [example](https://github.com/Disco-Dave/smart-constructor-example).

## Requirements
* [dotnet](https://dotnet.microsoft.com/download)

## How to
1. `$ git clone git@github.com:Disco-Dave/fsharp-smart-constructor-example.git`
2. `$ cd fsharp-smart-constructor-example`
3. `$ dotnet test`
