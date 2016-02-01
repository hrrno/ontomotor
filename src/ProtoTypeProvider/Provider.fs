namespace Samples.FSharp.HelloWorldTypeProvider

open System
open System.Reflection
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations

// This type defines the type provider. When compiled to a DLL, it can be added
// as a reference to an F# command-line compilation, script, or project.
[<TypeProvider>]
type SampleTypeProvider(config: TypeProviderConfig) as this = 

    // Inheriting from this type provides implementations of ITypeProvider 
    // in terms of the provided types below.
    inherit TypeProviderForNamespaces()

    let namespaceName = "Samples.HelloWorldTypeProvider"
    let thisAssembly = Assembly.GetExecutingAssembly()



    //type Type1

    // Make one provided type, called TypeN.
    let makeOneProvidedType (n:int) = 
        ProvidedTypeDefinition(thisAssembly,namespaceName,
                                   "Type" + string n,
                                   baseType = Some typeof<obj>)
    
    //new        //  …

    // Now generate 100 types
    let types = [ for i in 1 .. 100 -> makeOneProvidedType i ] 

    // And add them to the namespace
    do this.AddNamespace(namespaceName, types)

[<assembly:TypeProviderAssembly>] 
do()





//.