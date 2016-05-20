namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Informedica.GenUnits.lib")>]
[<assembly: AssemblyProductAttribute("Informedica.GenUnits.Lib")>]
[<assembly: AssemblyCompanyAttribute("halcwb")>]
[<assembly: AssemblyDescriptionAttribute("Basic units of measure library enabling calculating with values that have units")>]
[<assembly: AssemblyVersionAttribute("0.4.1")>]
[<assembly: AssemblyFileVersionAttribute("0.4.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.4.1"
    let [<Literal>] InformationalVersion = "0.4.1"
