#I "./Scripts"
#load "load-references-debug.fsx"

#time

open Microsoft.FSharp.Reflection



module Reflection =

    let toString (x:'a) = 
        match FSharpValue.GetUnionFields(x, typeof<'a>) with
        | case, _ -> case.Name

    let fromString (t:System.Type) (s:string) =
        match FSharpType.GetUnionCases t |> Array.filter (fun case -> case.Name = s) with
        |[|case|] -> Some(FSharpValue.MakeUnion(case,[||]))
        |_ -> None


module Continuation =

    open Informedica.Utilities.Nullable

    /// Utilitie function that allows continuation of general 
    /// creation fases of
    /// 1. null checking
    /// 2. canonicalization
    /// 3. validation
    /// 4. creation
    let create fIsNull fCanonicalize fIsValid fCtor ctor canonicalize isValid value =
        if value |> isNull then fIsNull
        else 
            let value = value |> canonicalize |> fCanonicalize
            if value |> isValid |> not then value |> fIsValid
            else  value |> ctor |> fCtor
    
    /// General application of continuation with
    /// Some for success and None if creation failed         
    let createSome ctor canonicalize isValid = 
        let fIsNull  = None
        let fCanonicalize = id 
        let fIsValid _ = None
        let fCtor v = v |> Some
        
        create fIsNull fCanonicalize fIsValid fCtor ctor canonicalize isValid 


module Exceptions =

    let optionToExn exn fn =
        match fn() with
        | Some r -> r
        | None -> raise exn


module Result =

    open Informedica.Utilities.Nullable

    module C = Continuation

    /// A Result is a success or failure
    /// The Success case has a success value, plus a list of messages
    /// The Failure case has just a list of messages
    type T<'TSuccess, 'TMessage> =
        | Success of 'TSuccess * 'TMessage list
        | Failure of 'TMessage list

    /// create a Success with no messages
    let succeed x =
        Success (x,[])

    /// create a Success with a message
    let succeedWithMsg x msg =
        Success (x,[msg])

    /// create a Failure with a message
    let fail msg =
        Failure [msg]

    /// A function that applies either fSuccess or fFailure 
    /// depending on the case.
    let either fSuccess fFailure = function
        | Success (x,msgs) -> fSuccess (x,msgs) 
        | Failure errors -> fFailure errors 

    /// merge messages with a result
    let mergeMessages msgs result =
        let fSuccess (x,msgs2) = 
            Success (x, msgs @ msgs2) 
        let fFailure errs = 
            Failure (errs @ msgs) 
        either fSuccess fFailure result

    /// given a function that generates a new RopResult
    /// apply it only if the result is on the Success branch
    /// merge any existing messages with the new result
    let bindR f result =
        let fSuccess (x,msgs) = 
            f x |> mergeMessages msgs
        let fFailure errs = 
            Failure errs 
        either fSuccess fFailure result

    let bindL result f = bindR f result

    /// infix version of bindL
    let (>>=) = bindL

    let (<<=) = bindR

    /// given a function wrapped in a result
    /// and a value wrapped in a result
    /// apply the function to the value only if both are Success
    let applyR f result =
        match f,result with
        | Success (f,msgs1), Success (x,msgs2) -> 
            (f x, msgs1 @ msgs2) |> Success 
        | Failure errs, Success (_,msgs) 
        | Success (_,msgs), Failure errs -> 
            errs @ msgs |> Failure
        | Failure errs1, Failure errs2 -> 
            errs1 @ errs2 |> Failure 

    /// infix version of apply
    let (<*>) = applyR

    /// given a function that transforms a value
    /// apply it only if the result is on the Success branch
    let liftR f result =
        let f' =  f |> succeed
        applyR f' result 

    /// given two values wrapped in results apply a function to both
    let lift2R f result1 result2 =
        let f' = liftR f result1
        applyR f' result2 

    /// given three values wrapped in results apply a function to all
    let lift3R f result1 result2 result3 =
        let f' = lift2R f result1 result2 
        applyR f' result3

    /// given four values wrapped in results apply a function to all
    let lift4R f result1 result2 result3 result4 =
        let f' = lift3R f result1 result2 result3 
        applyR f' result4

    /// infix version of liftR
    let (<!>) = liftR

    /// synonym for liftR
    let mapR = liftR

    /// given an RopResult, call a unit function on the success branch
    /// and pass thru the result
    let successTee f result = 
        let fSuccess (x,msgs) = 
            f (x,msgs)
            Success (x,msgs) 
        let fFailure errs = Failure errs 
        either fSuccess fFailure result

    /// given an RopResult, call a unit function on the failure branch
    /// and pass thru the result
    let failureTee f result = 
        let fSuccess (x,msgs) = Success (x,msgs) 
        let fFailure errs = 
            f errs
            Failure errs 
        either fSuccess fFailure result

    /// given an RopResult, map the messages to a different error type
    let mapMessagesR f result = 
        match result with 
        | Success (x,msgs) -> 
            let msgs' = List.map f msgs
            Success (x, msgs')
        | Failure errors -> 
            let errors' = List.map f errors 
            Failure errors' 

    /// given an RopResult, in the success case, return the value.
    /// In the failure case, determine the value to return by 
    /// applying a function to the errors in the failure case
    let valueOrDefault f result = 
        match result with 
        | Success (x,_) -> x
        | Failure errors -> f errors

    

    /// lift an option to a RopResult.
    /// Return Success if Some
    /// or the given message if None
    let failIfNone message = function
        | Some x -> succeed x
        | None -> fail message 

    /// given an RopResult option, return it
    /// or the given message if None
    let failIfNoneR message = function
        | Some rop -> rop
        | None -> fail message 

    let resultIs bf = function 
        | Failure _ -> bf |> not
        | Success _ -> bf

    let isFailure r = resultIs false r
    let isSuccess r = resultIs true r


    let getMessages = function
        | Success(_, msgs) | Failure msgs -> msgs

    let create isNullMsg canonicalization validation ctor value =
        let liftIsValid f b = function
            | Success(v, _) -> v |> f && b
            | Failure _ -> false
             
        let fIsNull = isNullMsg |> fail

        let canonicalize value =  
            canonicalization 
            |> List.fold (fun value (f, m) -> value |> liftR f |> mergeMessages [m]) (value |> succeed)

        let validate value =  
            validation 
            |> List.fold (fun (value, isValid) (f, m) -> 
                let isValid' = value |> liftIsValid f isValid
                if isValid' then (value, true)
                else 
                    (value |> mergeMessages [m], false)) (value, true)

        let fIsValid value = let (value', _) = value |> validate in value' |> getMessages |> Failure
        let isValid value  = let (_, valid)  = value |> validate in valid 

        let ctor' value = 
            let (c, m) = ctor
            value |> liftR c |> mergeMessages [m]

        C.create fIsNull id fIsValid id ctor' canonicalize isValid value


module Railway =
    
    type Result<'TSuccess, 'TFailure> =
        | Success of 'TSuccess
        | Failure of 'TFailure

    type Request<'TRequest> = 
        | Request of 'TRequest

    let succeed x = 
        Success x

    let fail x = 
        Failure x

    let getSuccess x = 
        match x with
        | Success s -> s |> Some
        | Failure _ -> None
        |> Option.get

    let bind switch =
        function
        | Success s -> s |> switch
        | Failure f -> Failure f  

    let (>>=) input switch = 
        bind switch input 

    let (>=>) switch1 switch2 = 
        switch1 >> (bind switch2)

    let switch f x = 
        f x |> Success

    let switchSome f m x =
        match x |> f with 
        | Some r -> r |> succeed
        | None -> m |> fail

    let doubleMap f1 f2 input =
        match input with
        | Success s -> Success (f1 s)
        | Failure f -> Failure (f2 f)

    // convert a normal function into a two-track function
    let map f = 
        doubleMap f id

    let tee f x = 
        f x |> ignore
        x

    // add two switches in parallel
    let plus addSuccess addFailure switch1 switch2 x = 
        match (switch1 x),(switch2 x) with
        | Success s1,Success s2 -> Success (addSuccess s1 s2)
        | Failure f1,Success _  -> Failure f1
        | Success _ ,Failure f2 -> Failure f2
        | Failure f1,Failure f2 -> Failure (addFailure f1 f2)

    let mapToTuple f x2 x1 =
        match x1, x2 |> f with 
        | Success s1, Success s2 -> (s1, s2) |> succeed 
        | Failure f1, Success _  -> f1 |> fail
        | Failure f1, Failure f2 -> f1 @ f2 |> fail
        | Success _ , Failure f2 -> f2 |> fail

    let mapToTriple f x2 x1 =
        match x1, x2 |> f with 
        | Success (s1, s2), Success s3 -> (s1, s2, s3) |> succeed 
        | Failure f1, Success _  -> f1 |> fail
        | Failure f1, Failure f2 -> f1 @ f2 |> fail
        | Success _ , Failure f2 -> f2 |> fail

    type RailwayBuilder () =

        member this.Return(x) = succeed x

        member this.Bind k = bind k
            

module StringBCL =

    let toString s = s.ToString()

    let empty (s: string) = System.String.IsNullOrWhiteSpace(s)

    let notEmpty = empty >> not

    let toUpper (s: string) = s.ToUpper()
    let toLower (s: string) = s.ToLower()

    let length (s: string) = s.Length

    let substring start length (s: string) = s.Substring(start, length)

    let trim (s: string) = s.Trim()

    /// Get the first character of a string
    /// as a string
    let firstStringChar = substring 0 1

    /// Return the rest of a string as a string
    let restString s = substring 1 ((s |> length) - 1) s

    let firstToUpper = firstStringChar >> toUpper
    let capitalize s = (s |> firstToUpper) + (s |> restString |> toLower)

    let letters = ['a'..'z'] @ ['A'..'Z'] |> List.map toString
    let isLetter s = List.exists (fun s' -> s' = s) letters

    let equalsCapInsens s1 s2 = s1 |> toLower |> trim = (s2 |> toLower |> trim) 


module RegexBCL =

    open System.Text.RegularExpressions

    let regex s = new Regex(s)

    let apply f (a: Regex) =
        a.ToString() |> f

    let value s = apply id s

    let replace s (rs: string) (re: Regex) = re.Replace(s, rs)

    let bind rx (f: string -> Regex) = rx |> value |> f

    // Ensure string has two uppercase letters
    let oneUpperCase = "(?=.*[A-Z])" |> regex
    // Ensure string has two uppercase letters
    let twoUpperCase = "(?=.*[A-Z].*[A-Z])" |> regex
    // Ensure string has one special case letter.  
    let oneSpecialCase = "(?=.*[!@#$&*])" |> regex
    // Ensure string has two digits.
    let oneDigit = "(?=.*[0-9])" |> regex
    // Ensure string has two digits.
    let twoDigits = "(?=.*[0-9].*[0-9])" |> regex
    // Ensure string has three lowercase letters.
    let threeLowerCase = "(?=.*[a-z].*[a-z].*[a-z])" |> regex

    let regexMatch (regex: Regex) s =
        (s, regex.Match(s).Success)

    let bindMatch sm f = 
        match sm with
        | (s, m) when m -> f s
        | _ -> sm

    let matchRegexList list s  =
        let (>>=) sm f = bindMatch sm f

        list |> List.fold(fun sm m -> sm >>= regexMatch m) (s, true) 


module WrappedValue =

    open Informedica.Utilities.Nullable

    open Result

    module Message =
        
        type T = 
            | NullValue

    type IWrappedValue<'T when 'T:> System.IComparable> =
        abstract member Value: 'T

    /// Create a wrapped value option
    /// 1) canonicalize the input first
    /// 2) If the validation succeeds, return Some of the given constructor
    /// 3) If the validation fails, return None
    /// Null values are never valid.
    let createSome (ctor: 'V -> 'W) canonicalize isValid  (value: 'V) = 
        if value |> isNull then None
        else
            let value' = canonicalize value
            if isValid value'
            then Some (ctor value') 
            else None

    let create = createSome

    let createResult ctor canonicalize validate value =
        if value |> isNull then Message.NullValue |> fail
        else 
            value 
            |> canonicalize
            >>= validate
            >>= ctor 

    /// Apply the given function to the wrapped value
    let apply f (v:IWrappedValue<_>) = 
        v.Value |> f 

    /// Get the wrapped value
    let value s = apply id s

    /// Equality 
    let equals left right = 
        (value left) = (value right)

    let equalsValue (wrapped: IWrappedValue<_>)  (value: _)= 
        wrapped.Value = value

    /// Comparison
    let compareTo left right = 
        (value left).CompareTo (value right)    

    /// Bind for composition
    let bind (value: IWrappedValue<_>) (f: _ -> IWrappedValue<_>) =
        value.Value |> f

    
module WrappedString =

    open System
    open Informedica.Utilities.Nullable

    open StringBCL
    open RegexBCL

    module WV = WrappedValue


    type SingleCapital = SingleCapital of string with
        interface WV.IWrappedValue<string> with
            member this.Value = let (SingleCapital s) = this in s

    type NumberString = NumberString of string with
        interface WV.IWrappedValue<string> with
            member this.Value = let (NumberString s) = this in s

    type NonNegativeNumberString = NonNegativeNumberString of string with
        interface WV.IWrappedValue<string> with
            member this.Value = let (NonNegativeNumberString s) = this in s

    let apply = WV.apply

    let singleLineTrimmed s =
        ("\s" |> regex) |> replace s " " |> trim
        
    let lengthValidator len (s: String) =
        s.Length <= len

    let isNotLargerThan len = lengthValidator len 
    let isLargerThan len s = lengthValidator len s |> not

    let length ws = ws |> WV.value |> length

    let singleLineStringOfLen ctor len s = 
        match s |> WV.create ctor singleLineTrimmed (lengthValidator len) with
        | Some s -> s |> Some
        | None -> None
    
    let stringIsNum s = try double s |> ignore; s |> Some with | _ -> None 

    let numStrIsNotNegative s = if s |> double >= 0. then s |> Some else None

    let createNumberString s = 
        match s |> singleLineTrimmed |> stringIsNum with
        | Some s -> s |> NumberString |> Some
        | None -> None

//    let createNonNegativeNumberString s = 
//        let create s' = 
//            match s' |> numStrIsNotNegative with
//            | Some s'' -> s'' |> NonNegativeNumberString |> Some
//            | None -> None
//        s |> createNumberString >>= create
//
//    let createSingleCapital s = s |> createString1 >>= (fun s -> s |> SingleCapital |> Some)

    module Messages =

        type T = 
            | StringIsNull
            | StringIsEmpty
            | StringLargerThan of int
            | StringSmallerThan of int
            | CreatedString of string


    let equalsCapInsens s1 s2 = s1 |> WV.value |> StringBCL.equalsCapInsens (s2 |> WV.value)

    let equalsStringCapInsens s2 s1 = s1 |> WV.value |> StringBCL.equalsCapInsens s2

    let (>>=) = Result.bindL
    let (<<=) = Result.bindR

    let toResult s =  
        s
        |> Reflection.toString
        |> Messages.CreatedString 
        |> Result.succeedWithMsg s
        
    /// Create a function that first does a null check 
    /// before processing the string
    let checkNull f s = 
        if s |> isNull then Messages.StringIsNull |> Result.fail
        else s |> f


    let createWithLength maxl ctor s = 
        let crt s =
            let s = s |> singleLineTrimmed 
            if s |> isLargerThan maxl then 
                maxl |> Messages.StringLargerThan |> Result.fail
            else
                s |> ctor |> toResult

        s |> checkNull crt

    let createCapitalized ctor s =
        if s |> StringBCL.empty then Messages.StringIsEmpty |> Result.fail
        else
            s
            |> singleLineTrimmed 
            |> StringBCL.capitalize 
            |> ctor 
            |> toResult


    let createCapitalizedWithLength ctor maxl apply s =
        let check s =
            if s |> isLargerThan maxl then Messages.StringLargerThan maxl |> Result.fail
            else s |> ctor |> Result.succeed 
        s 
        |> createCapitalized ctor
        >>= (fun r -> r |> apply check)


    module Validation =

        let checkNull s = 
            if s |> isNull then Messages.StringIsNull |> Result.fail
            else s |> Result.succeed
        
        let checkMinLength min ws =
            ws |> WV.apply (fun s -> s |> StringBCL.length > min)

        let checkMinLengthR min ws = 
            if ws |> checkMinLength min then
                ws |> Result.succeed
            else Messages.StringSmallerThan min |> Result.fail

        let checkMinMax min max s =
            match s |> StringBCL.length with
            | n when n < min -> Messages.StringSmallerThan min |> Result.fail
            | n when n > max -> Messages.StringLargerThan max  |> Result.fail
            | n when n >= min && n <= max -> s |> Result.succeed
            | _ -> failwith "Cannot match n"


    module SingleLineTrimmed = 
        
        type T = SingleLineTrimmed of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (SingleLineTrimmed s) = this in s

        //Create
        let create = checkNull (singleLineTrimmed >> SingleLineTrimmed >> Result.succeed)


    module StringMinMax = 

        /// A single line string with a min and a max length 
        /// So the string can contain any characters
        type T = StringMinMax of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (StringMinMax s) = this in s
        
        /// Create a string with a min (minimum length) and
        /// a max (maximum lenght). Wrap the result in a 
        /// Result type
        let create min max s = 
            s 
            |> Validation.checkNull
            >>= (singleLineTrimmed >> Result.succeed)
            >>= Validation.checkMinMax min max
            >>= (StringMinMax >> Result.succeed)

        /// Apply the function f that returns a string
        /// to a StringMinMax
        /// and return a StringMinMax
        let apply f = (WV.apply f) >> StringMinMax


    module TestStringMinMax =
        open Swensen.Unquote

        module SMM = StringMinMax

        let createMin2Max5 = SMM.create 2 5

        let ``Given min 2 and max 5 creating with string foo will pass`` =
            fun () ->
                test <@ "foo" |> createMin2Max5 |> Result.isSuccess @>
            
        let ``Given min 2 and max 5 creating with null will fail`` =
            fun () ->
                test <@ null |> createMin2Max5 |> Result.isFailure @>
            
        let ``Given min 2 and max 5 creating with empty string will fail`` =
            fun () ->
                test <@ "" |> createMin2Max5 |> Result.isFailure @>
            
        let ``Given min 2 and max 5 creating with string h will fail`` =
            fun () ->
                test <@ "h" |> createMin2Max5 |> Result.isFailure @>
            
        let ``Given min 2 and max 5 creating with string foobar will fail`` =
            fun () ->
                test <@ "foobar" |> createMin2Max5 |> Result.isFailure @>


    module String1 =

        type T = String1 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (String1 s) = this in s

        let create = String1 |> createWithLength 1

        let apply = WV.apply 


    module String5 =

        type T = String5 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (String5 s) = this in s

        let create = String5 |> createWithLength 5
        
        let apply = WV.apply 


    module String10 =

        type T = String10 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (String10 s) = this in s

        let create = String10 |> createWithLength 10

        let apply = WV.apply 


    module String20 =

        type T = String20 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (String20 s) = this in s

        let create = String20 |> createWithLength 20
        
        let apply = WV.apply 


    module String30 =

        type T = String30 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (String30 s) = this in s

        let create = String30 |> createWithLength 30
        
        let apply = WV.apply 


    module String50 =

        type T = String50 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (String50 s) = this in s

        let create = String50 |> createWithLength 50
        
        let apply = WV.apply 


    module String100 =

        type T = String100 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (String100 s) = this in s

        let create = String100 |> createWithLength 100
        
        let apply = WV.apply 


    module CapitalizedName =

        type T = CapitalizedName of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (CapitalizedName s) = this in s

        let create = checkNull (createCapitalized CapitalizedName)
            
        let apply = WV.apply


    module CapitalizedName10 =

        type T = CapitalizedName10 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (CapitalizedName10 s) = this in s

        let apply = WV.apply

        let create = createCapitalizedWithLength CapitalizedName10 10 apply
            

    module CapitalizedName20 =

        type T = CapitalizedName20 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (CapitalizedName20 s) = this in s

        let apply = WV.apply

        let create = createCapitalizedWithLength CapitalizedName20 20 apply
            

    module CapitalizedName30 =

        type T = CapitalizedName30 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (CapitalizedName30 s) = this in s

        let apply = WV.apply

        let create = createCapitalizedWithLength CapitalizedName30 30 apply
            

    module CapitalizedName50 =

        type T = CapitalizedName50 of string with
            interface WV.IWrappedValue<string> with
                member this.Value = let (CapitalizedName50 s) = this in s

        let apply = WV.apply

        let create = createCapitalizedWithLength CapitalizedName50 50 apply      


module WrappedDateTime =
    open System

    type T = WrappedDateTime of DateTime with
        interface WrappedValue.IWrappedValue<DateTime> with
            member this.Value = let (WrappedDateTime dt) = this in dt 

    let create = WrappedValue.create WrappedDateTime

    let value = WrappedValue.value


module ValueGenerator =

    open Informedica.Utilities.Math

    let genInfSeq fp fv =
        let rec s c = 
            seq { yield fv c;
                  let c = c + 1N
                  yield! c |> s }
        s 0N |> Seq.takeWhile(fun v -> fp(v))

    let genIncrSeq mn mx ic =
        if mx |> Option.isNone then Seq.empty
        else
            let mn = match mn with |Some v -> v |None -> ic
            let fp x = 
                match mx with
                | Some m -> x <= m
                | None -> true
            let fv c =
                mn + c * ic
            genInfSeq fp fv

    let genDecrSeq mn mx dc =
        let mn = match mn with |Some v -> v |None -> 0N
        let fp x =
            x >= mn
        let fv c =
            if c = 0N then mx
            else
                let i = if dc = 1N then c + 1N else c
                mx / (i * dc)
        genInfSeq fp fv

    let genDivSeq mn mx ic dv = 
        seq { for d in divisorsOfN dv do
                if (d / ic).Denominator = 1I && d > mn && d <= mx then 
                    yield Some d
                else yield None }
        |> Seq.filter(fun x -> x <> None)
        |> Seq.map(fun x -> match x with |Some v -> v |None -> failwith "Should have a value")

    let genValues min max incr decr div =
        printfn "generate values %A %A %A %A %A" min max incr decr div
        match min, max, incr, decr, div with
        | Some mn, Some mx, Some ic, Some dc, None -> 
            genDecrSeq min mx dc
            |> Seq.filter(fun dv -> [ mn..ic..mx ] |> List.exists(fun iv -> iv = dv))
        | _      , _      , Some ic, None,    None -> genIncrSeq min max ic
        | _      , Some mx, None,    Some dc, None -> genDecrSeq min mx dc
        | _,       _,       Some ic, _,       Some dv ->
            // Convert ic and dv to numerator with common denominator
            let ic, dv, d = 
                match [ic;dv] |> toNumListDenom with 
                |([i;c],d) -> (i, c, d)
                | _ -> failwith "Inconsistent match"
            // Get maximum and minimum values scaled to common denominator
            let mn = if min  = None then ic else min.Value * d
            let mx = if max  = None then dv else max.Value * d
            // Get list of divisors filtered by min, max and scaled back by denominator
            let dvl =
                seq { for d in divisorsOfN dv do
                         if (d / ic).Denominator = 1I && d >= mn && d <= mx then 
                            yield Some d
                         else yield None }

                |> Seq.filter(fun v -> v <> None)
                |> Seq.map(fun v -> v.Value)
                |> Seq.map(fun v -> v / d)
            // Filter list by decrement
            if decr = None then dvl
            else dvl |> Seq.filter(fun dv -> genDecrSeq (Some mn) mx decr.Value |> Seq.exists(fun v -> v = dv))

        | _ -> Seq.empty
        |> Seq.cache
           

module ValueSet =

    open System.Collections.Generic

    open Informedica.Utilities.Math

    type T = ValueSet of Set<BigRational>

    let toSet s = new Set<_>(s) 
    let emptySet = Set.empty

    let create s = s |> toSet |> ValueSet
    let getValues (ValueSet s) = s

    let one = [1N] |> create

    let setValues (ValueSet vs1) (ValueSet vs2) = 
        let vs1 = vs1 |> Seq.filter(fun x -> vs2 |> Seq.exists ((=) x))
        if vs1 |> Seq.length = 0 then vs2 else vs1 |> toSet 
        |> create

    let toList = getValues >> Seq.toList

    let calculate op = function
        | ValueSet s1, ValueSet s2 ->
            let s1 = new ResizeArray<_>(s1)
            let s2 = new ResizeArray<_>(s2)
            let s3 = new ResizeArray<_>()

            for x1 in s1 do
                for x2 in s2 do
                    s3.Add(x1 |> op <| x2) 
            new HashSet<_>(s3) |> toSet |> ValueSet

    type T with
        
        static member (*) (s1, s2) = calculate (*) (s1, s2)
        static member (/) (s1, s2) = calculate (/) (s1, s2)

        static member (+) (s1, s2) = calculate (+) (s1, s2)
        static member (-) (s1, s2) = calculate (-) (s1, s2)

        static member (=!) (result, expr) = result |> setValues expr 


module TestValueSet =
    open Swensen.Unquote
    module VS = ValueSet

    let toList = VS.toList

    let vsOne = VS.create [1N]
    let vsTwo = VS.create [2N]

    let vsTwoFour = VS.create [2N;4N]
    let vsSixEight = VS.create [6N;8N]

    let ``Multiplying 1 with 2 should equal 2`` =
        fun () -> 
            test<@vsOne * vsTwo = vsTwo@>

    let ``Divide 2 with 1 should equal 2`` =
        fun () -> 
            test<@vsTwo / vsOne = vsTwo@>
        
    let ``Adding 1 with 1 should equal 2`` =
        fun () -> 
            test<@vsOne + vsOne = vsTwo@>

    let ``Subtracting 2 with 1 should equal 1`` =
        fun () -> 
            test<@vsTwo - vsOne = vsOne@>
        
    let ``Multiplying 2,4 with 2,4 should equal 4,8,16`` =
        fun () -> 
            test<@vsTwoFour * vsTwoFour |> toList = [4N;8N;16N]@>

    let ``Divide 2,4 with 2,4 should equal 1/2,1,2`` =
        fun () -> 
            test<@vsTwoFour / vsTwoFour |> toList  = [1N/2N;1N;2N]@>
        
    let ``Adding 2,4 with 2,4 should equal 4,6,8`` =
        fun () -> 
            test<@vsTwoFour + vsTwoFour|> toList  = [4N;6N;8N]@>

    let ``Subtracting 6,8 with 2,4 should equal 2,4,6`` =
        fun () -> 
            test<@vsSixEight - vsTwoFour|> toList  = [2N;4N;6N]@>
        
    let ``Setting value set 2,4 with 1..10 equals value set 2,4`` =
        fun () -> 
            test<@vsTwoFour |> VS.setValues ([1N..10N] |> VS.create) = vsTwoFour@>
        
    let ``Setting value set 2,4 with 2 equals value set 2`` =
        fun () -> 
            test<@vsTwoFour |> VS.setValues ([2N] |> VS.create) = vsTwo@>
        
    let run() =
        [
            ``Multiplying 1 with 2 should equal 2``
            ``Divide 2 with 1 should equal 2``
            ``Multiplying 2,4 with 2,4 should equal 4,8,16``
            ``Divide 2,4 with 2,4 should equal 1/2,1,2``
            ``Adding 2,4 with 2,4 should equal 4,6,8``
            ``Subtracting 6,8 with 2,4 should equal 2,4,6``
            ``Setting value set 2,4 with 1..10 equals value set 2,4``
            ``Setting value set 2,4 with 2 equals value set 2``
        ] |> List.iter (fun t -> t())


module Multipliers =

    let one = 1N

    let kilo = 1000N
    let deci = 1N / 10N
    let centi = deci / 10N
    let milli = 1N / kilo
    let micro = milli / kilo                                                                                            
    let nano = micro / kilo

    let second = 1N
    let minute = 60N * second
    let hour = minute * minute
    let day = 24N * hour
    let week = 7N * day
    let month = 4N * week
    let year = 365N * day 

    let toBase m v  = v * m
    let toUnit m v  = v / m
    let toValue m v = let m = one in v / m


module TestMultipliers =
    open Swensen.Unquote
    module MP = Multipliers

    let ``Converting value to base and back to unit equals the value`` =
        fun () -> 
            let c = MP.toBase MP.kilo >> MP.toUnit MP.kilo >> MP.toValue MP.kilo
            test<@ 10N |> c = 10N @>

    let run() =
        [
            ``Converting value to base and back to unit equals the value``
        ] |> List.iter (fun t -> t())


module Unit =  

    module SBCL = StringBCL
    module WS = WrappedString
    module MP = Multipliers

    let (>>=) = Result.bindL
    let (<!>) = Result.liftR
    let (<*>) = Result.applyR

    module Messages =

        type T =
            | UnitIsRequired
            | CannotCreateGroupName of string
            | CannotCreateName of string
            | CannotCreateAbbreviation of string
            | MultiplierNotPositive of BigRational
            | CreatedGroupName of string
            | CreatedName of string
            | CreatedAbbreviation of string
            | CouldNotFindUnit of string

        [<Literal>]
        let message = "Could not find "
        
        let unitNotFound s = message + "unit: " + s
        let weightNotFound s = message + "weight unit: " + s
        let massNotFound s = message + "mass unit: " + s
        let timeNotFound s = message + "time unit: " + s
        let distanceNotFound s = message + "distance unit: " + s
        let adjustNotFound s = message + "adjust unit: " + s

    module Dto =

        [<AllowNullLiteralAttribute>]
        type T () = 
            member val GroupName: string = null with get, set
            member val Name: string = null with get, set
            member val NameList: string list = [] with get, set
            member val Abbreviation: string = null with get, set
            member val AbbreviationList: string list = [] with get, set
            member val Multiplier = 0N with get, set

        let createNew () = new T()

        let create gr n mult ab = 
            let dto = new T()
            dto.GroupName <- gr
            dto.Name <- n
            dto.Multiplier <- mult
            dto.Abbreviation <- ab
            dto


    module GroupName = 

        type T = GroupName of WS.CapitalizedName30.T

        let create s = 
            let map = function
                | WS.Messages.CreatedString _ -> Messages.CreatedGroupName s
                | WS.Messages.StringIsEmpty -> Messages.CannotCreateGroupName "empty string"
                | WS.Messages.StringIsNull -> Messages.CannotCreateGroupName "null value"
                | WS.Messages.StringLargerThan _ -> Messages.CannotCreateGroupName "string larger than 30"
                | WS.Messages.StringSmallerThan n -> Messages.CannotCreateGroupName ("string smaller than " + string n)

            GroupName 
            <!> WS.CapitalizedName30.create s 
            |> Result.mapMessagesR map

        let apply f (GroupName n) = n |> WS.CapitalizedName30.apply f

        let get = apply id
        let change f x = x |> apply f |> create

        module GroupNameTests =
            
            open Swensen.Unquote

            // Groupname cannot be created with an empty or null string
            test <@ create "" |> Result.isFailure @>
            test <@ create null |> Result.isFailure @>

            // Groupname should be at least one character long
            // and smaller than 30 characters
            test <@ create "1" |> Result.isSuccess @>
            test <@ "s" |> String.replicate 31 |> create |> Result.isFailure @>
            test <@ "s" |> String.replicate 30 |> create |> Result.isSuccess @>

            // Groupname starts with a capital and
            // the rest is lower case
            test <@ "GROUPNAME" 
                    |> create 
                    |> Result.either (fun (gn, _) -> gn |> get = "Groupname")
                                     (fun _ -> false) @>
            test <@ "GroupName" 
                    |> create 
                    |> Result.either (fun (gn, _) -> gn |> get = "Groupname")
                                     (fun _ -> false) @>

    module Name = 

        type T = Name of WS.String20.T

        let create s = 

            let map = function
                | WS.Messages.CreatedString _ -> Messages.CreatedName s
                | WS.Messages.StringIsEmpty -> Messages.CannotCreateName "empty string"
                | WS.Messages.StringIsNull -> Messages.CannotCreateName "null value"
                | WS.Messages.StringLargerThan _ -> Messages.CannotCreateName "string larger than 20"
                | WS.Messages.StringSmallerThan n -> Messages.CannotCreateGroupName ("string smaller than " + string n)

            let nameOrFail s = 
                match s |> WS.String20.create with
                | Result.Success (s20, msgs) -> 
                    if s20 |> WS.Validation.checkMinLength 1 then s20 |> Result.succeed
                    else Messages.CannotCreateName "string smaller than 1" |> Result.fail
                | Result.Failure errs -> errs |> Result.Failure |> Result.mapMessagesR map

            Name 
            <!> nameOrFail s

        let apply f (Name n) = n |> WS.String20.apply f

        let get = apply id
        let change f x = x |> apply f |> create

        module NameTests =
            
            open Swensen.Unquote

            // A name cannot be created with an empty
            // or a null string
            test <@ null |> create |> Result.isFailure @>
            test <@ ""   |> create |> Result.isFailure @> 

            // A name is a single line string smaller than 20
            test <@ "s" 
                    |> String.replicate 21 
                    |> create 
                    |> Result.isFailure @>
            test <@ @"this is a 
                      multiline string" 
                    |> create
                    |> Result.isFailure @>

            // a string 20 characters or less is valid
            test <@ "validname" |> create |> Result.isSuccess @>

            // a name can be with lowercase or not
            test <@ "lowercase" 
                    |> create 
                    |> Result.either (fun (n, _) -> n |> get = "lowercase")
                                     (fun _ -> false) @>
            test <@ "Capitalized" 
                    |> create 
                    |> Result.either (fun (n, _) -> n |> apply ((=) "Capitalized"))
                                     (fun _ -> false) @>

    module Abbreviation = 

        type T = Abbreviation of WS.String10.T

        let create s = 
            let map = function
                | WS.Messages.CreatedString _ -> Messages.CreatedAbbreviation s
                | WS.Messages.StringIsEmpty -> Messages.CannotCreateAbbreviation "empty string"
                | WS.Messages.StringIsNull -> Messages.CannotCreateAbbreviation "null value"
                | WS.Messages.StringLargerThan _ -> Messages.CannotCreateAbbreviation "string larger than 10"
                | WS.Messages.StringSmallerThan _ -> Messages.CannotCreateAbbreviation "string smaller than 1"

            let validateLargerThanOne ws =
                match ws |> WS.apply (fun s -> s |> String.length > 1) with
                | true  -> ws |> Result.succeed
                | false -> WS.Messages.StringSmallerThan 1 |> Result.fail

            s 
            |> WS.String10.create 
            >>= validateLargerThanOne
            >>= (Abbreviation >> Result.succeed)
            |> Result.mapMessagesR map

        let apply f (Abbreviation n) = n |> WS.String10.apply f

        let get = apply id
        let change f x = x |> apply f |> create

        module AbbreviationTests =
            
            open Swensen.Unquote

            // An abbreviation cannot be created with a null
            // or empty string
            test <@ null |> create |> Result.isFailure @>
            test <@ ""   |> create |> Result.isFailure @>

            // Abbr is a valid name for an Abbreviation
            test <@ "Abbr" |> create |> Result.isSuccess @>

            // Abbreviations can be lowercase or uppercase
            test <@ "UPPER" 
                    |> create
                    |> Result.either (fun (a, _) -> a |> apply ((=) "UPPER"))
                                     (fun _ -> false) @>
            test <@ "lower" 
                    |> create
                    |> Result.either (fun (a, _) -> a |> apply ((=) "lower"))
                                     (fun _ -> false) @>

            // However it cannot be a multiline
            test <@ @"multi
                    line"
                    |> create
                    |> Result.isFailure @>

    type T = 
        { 
            Group: GroupName.T
            Name: Name.T * Name.T list
            Abbreviation: Abbreviation.T * Abbreviation.T list
            Multiplier: BigRational
        }

    let create gr n mult ab = { Group = gr; Name = n, []; Abbreviation = ab, []; Multiplier = mult }

    let dtoToUnit (dto: Dto.T) =
        if dto = null then Messages.UnitIsRequired |> Result.fail 
        else
            let multiplier =
                if dto.Multiplier < 0N then 
                    dto.Multiplier
                    |> Messages.MultiplierNotPositive
                    |> Result.fail
                else dto.Multiplier |> Result.succeed

            create
            <!> GroupName.create dto.GroupName
            <*> Name.create dto.Name
            <*> multiplier
            <*> Abbreviation.create dto.Abbreviation


    let applyToUnit f (u: T) = u |> f

    let getMultiplier = applyToUnit (fun u -> u.Multiplier)
    let getGroupName = applyToUnit (fun u -> u.Group |> GroupName.get)
    let getName = applyToUnit (fun u -> let n, nl = u.Name in n |> Name.get, nl |> List.map Name.get)
    let getAbbreviation = applyToUnit (fun u -> let a, al = u.Abbreviation in a |> Abbreviation.get, al |> List.map Abbreviation.get)

    let convert f u v = v |> f (u |> getMultiplier)

    let toUnit  = convert MP.toUnit 
    let toBase  = convert MP.toBase
    let toValue = convert MP.toValue

    let eqGroup u1 u2 = u1 |> getGroupName = (u2 |> getGroupName)

    let unitToUnit u1 u2 = 
        if eqGroup u1 u2 then u1 |> toBase >> (u2 |> toUnit)
        else failwith "Cannot convert group"

    module UnitTests =
        
        open Swensen.Unquote

        // A unit cannot be created with a null dto
        test <@ null |> dtoToUnit |> Result.isFailure @>

        // A unit cannot be created with a new/empty dto
        test <@ let dto = Dto.createNew ()
                dto |> dtoToUnit |> Result.isFailure @>

        // A unit has to have a name
        test <@ let dto = Dto.createNew ()
                dto 
                |> dtoToUnit 
                |> Result.getMessages 
                |> List.exists (fun m -> match m with 
                                         | Messages.CannotCreateName _ -> true 
                                         | _ -> false) @>

        // A unit has to have an abbreviation
        test <@ let dto = Dto.createNew ()
                dto.Name <- "unitname"
                dto 
                |> dtoToUnit 
                |> Result.getMessages 
                |> List.exists (fun m -> match m with 
                                         | Messages.CannotCreateAbbreviation _ -> true 
                                         | _ -> false) @>

    module Units =

        let getUnitFromResult = Result.valueOrDefault (fun _ -> failwith "Unexpected error in Unit module")
        let mapDtoToUnit = dtoToUnit >> getUnitFromResult

        [<Literal>]
        let generalGroup = "General"

        [<Literal>]
        let countGroup = "Count"
        let createCount = Dto.create countGroup
        let count =  createCount "Times" MP.one "X" |> mapDtoToUnit
        let countUnits = [count] 

        [<Literal>]
        let massGroup = "Mass"
        let createMass = Dto.create massGroup
        let kiloGram = createMass "KiloGram" MP.kilo "kg" |> mapDtoToUnit
        let gram = createMass "Gram" MP.one "g" |> mapDtoToUnit
        let milliGram = createMass "MilliGram" MP.milli "mg" |> mapDtoToUnit
        let microGram = createMass "MicroGram" MP.micro "mcg" |> mapDtoToUnit
        let nanoGram = createMass "NanoGram" MP.nano "nanog" |> mapDtoToUnit
        let massUnits = [kiloGram;gram;milliGram;microGram;nanoGram]

        [<Literal>]
        let molarGroup = "Molar"
        let createMolar = Dto.create molarGroup
        let mol = createMass "Mol" MP.one "mol" |> mapDtoToUnit
        let milliMol = createMass "MilliMol" MP.milli "mmol" |> mapDtoToUnit
        let molarUnits = [mol;milliMol] 

        [<Literal>]
        let weightGroup = "Weight"
        let createWeight = Dto.create weightGroup
        let weightKg = createWeight "KiloGram" MP.kilo "kg" |> mapDtoToUnit
        let weightGram = createWeight "Gram" MP.one "g" |> mapDtoToUnit
        let weightUnits = [weightKg;weightGram] 

        [<Literal>]
        let bsaGroup = "BSA"
        let createBSA = Dto.create bsaGroup
        let bsa = createBSA "SquareMeter" MP.kilo "m^2" |> mapDtoToUnit
        let bsaUnits = [bsa]

        [<Literal>]
        let volumeGroup = "volumeUnits"
        let createVolume = Dto.create volumeGroup
        let liter = createVolume "Liter" MP.one "l" |> mapDtoToUnit
        let deciLiter = createVolume "DeciLiter" MP.deci "dl" |> mapDtoToUnit
        let milliLiter = createVolume "MilliLiter" MP.milli "ml" |> mapDtoToUnit
        let microLiter = createVolume "MicroLiter" MP.micro "mcl" |> mapDtoToUnit
        let volumeUnits = [liter;deciLiter;milliLiter;microLiter]

        [<Literal>]
        let timeGroup = "Time"
        let createTime = Dto.create timeGroup
        let second = createTime "Second" MP.one "sec"  |> mapDtoToUnit
        let minute = createTime "Minute" MP.minute "min"  |> mapDtoToUnit
        let hour = createTime "Hour" MP.hour "hr"  |> mapDtoToUnit
        let day = createTime "Day" MP.day "day"  |> mapDtoToUnit
        let week = createTime "Week" MP.week "week"  |> mapDtoToUnit
        let month = createTime "Month" MP.month "mo"  |> mapDtoToUnit
        let year = createTime "Year" MP.year "yr"  |> mapDtoToUnit
        let timeUnits = [second;minute;hour;day;week;month;year] 

        [<Literal>]
        let distanceGroup = "Distance"
        let createDistance = Dto.create distanceGroup
        let meter = createDistance "Meter" MP.one "m" |> mapDtoToUnit
        let centimeter =createDistance "CentiMeter" MP.centi "cm" |> mapDtoToUnit
        let distanceUnits = [meter;centimeter]

        let units = [countUnits;massUnits;molarUnits;weightUnits;bsaUnits;volumeUnits;timeUnits;distanceUnits]

        let hasName s u = 
            let eqs = SBCL.equalsCapInsens s
            let (n, ns) = getName u
            n |> eqs || ns |> List.exists (SBCL.toLower >> SBCL.trim >> eqs)

        let find us s = 
            let u =
                us 
                |> List.collect id
                |> List.tryFind (hasName s)
            match u with 
            | Some u -> u |> Result.succeed
            | None -> s |> Messages.CouldNotFindUnit |> Result.fail

        let fromString = find units

        let weightFromString = find [weightUnits]    
        let distanceFromString = find [distanceUnits]
        let adjustFromString = find [weightUnits;bsaUnits]
        let timeFromString = find [timeUnits]

        let createFromString s =
            match s |> fromString with
            | Result.Success (r, msgs) -> (r, msgs) |> Result.Success
            | Result.Failure _ -> 
                let r = Dto.create generalGroup s MP.one s |> dtoToUnit
                match r with
                | Result.Failure errs -> errs |> Result.Failure
                | Result.Success (u, msgs) -> (u, msgs) |> Result.Success 

        let isGroup gr u = u |> getGroupName |> SBCL.equalsCapInsens gr
        let isTime = isGroup timeGroup
        let isVolume = isGroup volumeGroup
        let isAdjust u = u |> isGroup weightGroup || (u |> isGroup bsaGroup)
         

module TestUnit =
    open Swensen.Unquote
    module UN = Unit

    let resultIsFailure = Result.isFailure
    let resultIsSuccess = Result.isSuccess

    let ``Creating a unit with an empty dto will fail`` =
        fun () ->
            let dto = UN.Dto.createNew ()
            test <@ dto |> UN.dtoToUnit |> resultIsFailure @>

    let ``Creating a unit mass kilogram multiplier 1 abbreviation kg will succeed`` =
        fun () ->
            let dto = UN.Dto.createNew ()
            dto.GroupName <- "mass"
            dto.Name <- "kilogram"
            dto.Abbreviation <- "kg"
            dto.Multiplier <- 1N
            test <@ dto |> UN.dtoToUnit |> resultIsSuccess @>

    let ``Converting 2 kilo to basse gram equals 2000`` =
        fun () ->
            test<@ 2N |> UN.toBase UN.Units.kiloGram = 2000N @>

    let ``Converting 2000 gram to to unit kilo equals 2`` =
        fun () ->
            test<@ 2000N |> UN.toUnit UN.Units.kiloGram = 2N @>

    let ``Converting 2 kilo to kilo equals 2`` =
        fun () ->
            test<@ 2N |> UN.toValue UN.Units.kiloGram = 2N @>

    let ``Converting 1 milliGram to microGram equals 1000`` =
        fun () ->
            test<@ 1N |> UN.unitToUnit UN.Units.milliGram UN.Units.microGram = 1000N @>

    let run() =
        [
            ``Creating a unit with an empty dto will fail``
            ``Creating a unit mass kilogram multiplier 1 abbreviation kg will succeed``
            ``Converting 2 kilo to basse gram equals 2000``
            ``Converting 2000 gram to to unit kilo equals 2``
            ``Converting 2 kilo to kilo equals 2``
            ``Converting 1 milliGram to microGram equals 1000``
        ] |> List.iter (fun t -> t())


module ValuesUnit =

    open Informedica.Utilities

    module VS = ValueSet
    module MP = Multipliers
    module UN = Unit

    type T = ValuesUnit of VS.T * UN.T

    let create u v = (v, u) |> ValuesUnit

    let get (ValuesUnit(v, u)) = v, u
    let getValueSet = get >> fst
    let getUnit  = get >> snd

    let convert c vu =
        let vs, u = vu |> getValueSet, vu |> getUnit 

        vs
        |> VS.getValues 
        |> Seq.map (c u)
        |> VS.create

    let toBaseValueSet = convert UN.toBase 
    let toUnitValueSet = convert UN.toUnit 
    let toValueValueSet = convert UN.toValue

//    let setValueSet vs vu  = let (v, u) = vu |> get in (v |> VS.setValues vs, u) |> ValuesUnit
    let withValueSet vs vu = let (_, u) = vu |> get in (vs, u) |> ValuesUnit

    let withUnit u v = v |> VS.create |> create u

    let toOneUnit vu = withUnit (vu |> getUnit) [1N] 

    let oneCountUnit = [1N] |> withUnit UN.Units.count

    let convertToUnit u vu = (vu |> toBaseValueSet, u) |> ValuesUnit |> toUnitValueSet |> create u

    let unitGroupEqs (ValuesUnit(_, u1)) (ValuesUnit(_, u2)) = u1.Group = u2.Group
    let isCount vu = unitGroupEqs vu oneCountUnit

    let getUnitValues = toValueValueSet >> VS.toList
    let getBaseValues = toBaseValueSet >> VS.toList

    let setValues vu1 vu2 = 
        let bv = vu2 |> toBaseValueSet |> VS.setValues (vu1 |> toBaseValueSet)
        let uv = vu2 |> withValueSet bv |> toUnitValueSet   
        vu2 |> withValueSet uv

    let toString x =
        let v, u = x |> getUnitValues, x |> getUnit
        if v.Length = 1 then v.Head |> string else ""
        + " " + (u |> UN.getName |> fst)

    let fromString s =
        let toUv v u = 
            match u |> UN.Units.fromString with
            | Result.Success (u, _) -> [v |> Parsing.stringToBigRational] |> withUnit u |> Some
            | Result.Failure _ -> None

        match s |> Text.splitAtSpace with
        | [] -> None
        | u::[]    -> toUv "1" u 
        | v::u::[] -> toUv v u
        | _ -> None


module TestValuesUnit =

    open Swensen.Unquote
    module VS = ValueSet
    module UN = Unit
    module VU = ValuesUnit
    
    let twoKilo = [2N] |> VU.withUnit UN.Units.kiloGram
    let thousandMicroGram = [1000N] |> VU.withUnit UN.Units.microGram

    let ``Converting 2000 gram to to unit kilo equals 2 kilo`` =
        fun () ->
            test<@ [2000N] |> VU.withUnit UN.Units.gram |> VU.convertToUnit UN.Units.kiloGram = twoKilo @>

    let ``Converting 1 milliGram to microGram equals 1000 microGram`` =
        fun () ->
            test<@ [1N] |> VU.withUnit UN.Units.milliGram |> VU.convertToUnit UN.Units.microGram = thousandMicroGram @>

    let ``One count unit belongs to unit group count`` =
        fun () ->
            test<@ VU.oneCountUnit |> VU.isCount @>

    let run() =
        [
            ``Converting 2000 gram to to unit kilo equals 2 kilo``
            ``Converting 1 milliGram to microGram equals 1000 microGram``
        ] |> List.iter (fun t -> t())


module ValuesUnitCombi =

    open Informedica.Utilities

    module VS = ValueSet
    module UN = Unit
    module VU = ValuesUnit

    type T = ValuesUnitCombi of VU.T * (Operator * VU.T) list 
    and Operator = Times | Per

    let create ul u v = 
        (v |> VU.withUnit u, ul) |> ValuesUnitCombi

    let empty u = create [] u [] 
    let withUnit u v = v |> create [] u

    let get (ValuesUnitCombi(vu, us)) = vu, us

    let cont op v u cvu =
        let vu, us = get cvu
        (vu, us @ [(op, [v] |> VU.withUnit u)]) |> ValuesUnitCombi 

    let per   = cont Per
    let times = cont Times

    let perValueUnit vu vuc =
        let vu', us = get vuc
        (vu', us @ [(Per, vu)]) |> ValuesUnitCombi 

    let getUnits (ValuesUnitCombi(vu, oul)) =
        (Times, vu |> VU.getUnit)::(oul |> List.map(fun (o, vu) -> o, vu |> VU.getUnit)) 

    let eqUnits x1 x2 =
        let us1, us2 = x1 |> getUnits, x2 |> getUnits
        if us1.Length <> us2.Length then false
        else
            us1 
            |> List.fold2(fun b (o1, u1) (o2, u2) -> 
                b && o1 = o2 && u1 |> UN.eqGroup u2) true us2      

    let calc f x =
        let f' = fun acc x -> 
            let o, v = x 
            match o with 
            | Times -> acc * (v |> f)  
            | Per   -> acc / (v |> f) 

        let vu, us = x |> get
        us |> List.fold f' (vu |> f)

    let toBaseValueSet = calc VU.toBaseValueSet
    let toUnitValueSet = calc VU.toUnitValueSet
    let toValueValueSet = calc VU.toValueValueSet

    let getBaseValues = toBaseValueSet >> VS.toList
    let getUnitValues = toValueValueSet >> VS.toList

    let setValues cv1 cv2 =
        let b1 = cv1 |> toBaseValueSet
        let b2 = cv2 |> toBaseValueSet
        let vs = b2 |> VS.setValues b1 
        let vu, ul = cv2 |> get
        let vs = (vu |> VU.withValueSet vs, ul) |> ValuesUnitCombi |> toUnitValueSet
        (vu |> VU.withValueSet vs, ul) |> ValuesUnitCombi

    let withValueSet vs cv =
        let vu, us = cv |> get
        (vu |> VU.withValueSet vs, us) |> ValuesUnitCombi

    let convertToUnit x1 x2 = 
        let x = x1 |> withValueSet (x2 |> toBaseValueSet)
        x |> withValueSet (x |> toUnitValueSet)

    let setRange f v cv = 
        cv |> withValueSet (cv |> toValueValueSet |> f v)

    let eval x =
        let vu, us = x |> get
        let vs = x |> toBaseValueSet

        let sort xs =
            xs |> List.sortWith(fun x1 x2 -> 
                let op1, vu1 = x1
                let op2, vu2 = x2 
                match op1, op2 with
                | Times, Times -> if vu1 |> VU.toBaseValueSet > (vu2 |> VU.toBaseValueSet) then -1 else 0
                | Times, Per   -> -1
                | Per,  Times  -> +1
                | Per,  Per    -> 0)

        let eqs x1 x2 =
            let op1, vu1 = x1
            let op2, vu2 = x2
            let opeq = op1 = op2
            let greq = vu1 |> VU.unitGroupEqs vu2
            (opeq |> not) && greq 

        let rec simplify acc list = 
            let remCount xs = 
                xs |> List.filter(fun x -> let (_, vu) = x in vu |> VU.isCount |> not) 
                
            let rec remove i l =
                match i, l with
                | 0, x::xs -> xs
                | i, x::xs -> x::remove (i - 1) xs
                | i, [] -> failwith "index out of range"

            match list with
            | [] -> 
                let acc = acc |> remCount |> sort
                match acc with
                | [(Per, _)] -> (Times, VU.oneCountUnit)::acc
                | _          -> acc
            | x::xs -> 
                match xs |> List.tryFindIndex (eqs x) with
                | Some i -> 
                    xs |> remove i |> simplify acc
                | None -> 
                    let x = let (o, vu) = x in o, vu |> VU.toOneUnit
                    xs |> simplify (acc @ [x])
                    
        match simplify [] ((Times, vu)::us) with
        | [] -> 
            vs |> VS.toList 
            |> withUnit UN.Units.count
        | x::xs -> 
            let vu = let (o, vu) = x in (vu |> VU.withValueSet vs)
            let vs = (vu, xs) |> ValuesUnitCombi |> toUnitValueSet
            (vu |> VU.withValueSet vs, xs) |> ValuesUnitCombi

    let operateOn op x1 x2 =
        let operate op1 ov =
            let op2, v = ov
            match op1, op2 with
            | Per, Per -> (Times, v)
            | _ -> (op2, v)

        let vu1, us1 = x1 |> get
        let vu2, us2 = x2 |> get
        let us2 = us2 |> List.map (operate op)

        (vu1, us1 @ (op, vu2)::us2) 
        |> ValuesUnitCombi
        |> eval
    
    let multiply = operateOn Times
    let divide   = operateOn Per

    let addSubtr op x2 x1 =
        if x1 |> eqUnits x2 then
            let v = x1 |> toBaseValueSet |> (op) <| (x2 |> toBaseValueSet)
            x2 |> withValueSet (x2 |> withValueSet v |> toUnitValueSet)
        else failwith "Cannot add or subtract with different units"

    let add =      addSubtr (+)
    let subtract = addSubtr (-)

    let toString x =
        let toStr str ou =
            let o, u = ou
            let m =
                match o with
                | Per -> " / "
                | Times -> " * "
            str + m + (u |> VU.toString)

        let vu, ul = x |> get
        (vu |> VU.toString) + (ul |> List.fold toStr "")

    let fromString s =
        let toValU = List.map VU.fromString
        let plusSpace s1 s2 = s1 + " " + s2 

        let rec parse ul acc =
            match ul with
            | [] -> acc
            | (Some u)::rest ->
                acc |> perValueUnit u |> parse rest 
            | None::rest -> acc |> parse rest

        match s |> Text.splitAtSpace with
        | v::rest ->
            match rest |> List.fold plusSpace "" |> Text.textSplitAtChar '/' with
            | u::units -> 
                match u |> UN.Units.fromString with
                | Result.Success (u', _) -> 
                    [v |> Parsing.stringToBigRational] |> withUnit u' 
                    |> parse (units |> toValU) |> Some
                | Result.Failure _ -> None
            | _ -> None
        | _ -> None

    type T with

        member this.ToString = this |> toString

        static member (*) (x1, x2) = x2 |> multiply x1
        static member (/) (x1, x2) = x2 |> divide x1

        static member (+) (x1, x2) = x1 |> add x2
        static member (-) (x1, x2) = x1 |> subtract x2

        static member (=!) (result, expr) = result |> setValues expr 


module TestValuesUnitCombi =
    open Swensen.Unquote
    module VS = ValueSet
    module UN = Unit
    module VC = ValuesUnitCombi

    let emptyMg = [] |> VC.withUnit UN.Units.milliGram
    let tenMilliGramPerDay = [10N] |> VC.withUnit UN.Units.milliGram |> VC.per 1N UN.Units.day
    let fiveMilliGram = [5N] |> VC.withUnit UN.Units.milliGram

    test<@ tenMilliGramPerDay |> VC.toBaseValueSet |> VS.toList = [1N/8640000N] @>

    let baseOfTenMilliGramPerDay = 
        tenMilliGramPerDay 
        |> VC.toBaseValueSet 
        |> VS.toList 
        |> VC.withUnit UN.Units.milliGram |> VC.per 1N UN.Units.day

    test<@ baseOfTenMilliGramPerDay |> VC.toUnitValueSet |> VS.toList = [10N] @>

    let twoTimesPerDay = [2N] |> VC.withUnit UN.Units.count |> VC.per 1N UN.Units.day

    test<@ tenMilliGramPerDay / twoTimesPerDay = fiveMilliGram @>
    test<@ tenMilliGramPerDay / fiveMilliGram = twoTimesPerDay @>

    tenMilliGramPerDay / fiveMilliGram 
    tenMilliGramPerDay / twoTimesPerDay 

    fiveMilliGram + fiveMilliGram
    fiveMilliGram - fiveMilliGram
    tenMilliGramPerDay + tenMilliGramPerDay |> VC.toString

    twoTimesPerDay * fiveMilliGram

    let fiveGamma = [5N] |> VC.withUnit UN.Units.microGram |> VC.per 1N UN.Units.weightKg |> VC.per 1N UN.Units.minute
    let oneHour = [1N] |> VC.withUnit UN.Units.hour
    let sixtyMinute = [60N] |> VC.withUnit UN.Units.minute

    (oneHour * fiveGamma) / sixtyMinute |> VC.toBaseValueSet
    (oneHour * fiveGamma) / sixtyMinute 
    fiveGamma |> VC.toBaseValueSet

    let total12MilliGramPerDay = [12N] |> VC.withUnit UN.Units.milliGram |> VC.per 1N UN.Units.day
    let freq1to12PerDay = [1N..12N] |> VC.withUnit UN.Units.count |> VC.per 1N UN.Units.day
    let quantity1to10MilliGram = [1N..10N] |> VC.withUnit UN.Units.milliGram

    quantity1to10MilliGram =! (total12MilliGramPerDay / freq1to12PerDay)
    freq1to12PerDay =! (quantity1to10MilliGram =! (total12MilliGramPerDay / freq1to12PerDay))


module Variable =

    open Informedica.Utilities

    module VG = ValueGenerator
    module UN = Unit
    module VS = ValueSet
    module VU = ValuesUnit
    module VC = ValuesUnitCombi

    type Name = Name of string | NoName

    type Range =
        {
            Min: BigRational option
            Max: BigRational option
            Incr: BigRational option
            Unit: UN.T * (VC.Operator * VU.T) list
        }

    let createRange u ul = { Min = None; Max = None; Incr = None; Unit = u, ul } 
        
    type Values = | Values of VC.T | Range of Range | Empty

    let withUnit u vs = 
        match vs with
        | [] -> createRange u [] |> Range
        | _ -> vs |> VC.withUnit u |> Values

    let per v u = function
        | Values vc -> vc |> VC.per v u |> Values
        | Range r ->
            let ou = VC.Per, [v] |> VU.withUnit u 
            { r with Unit = let u, ul = r.Unit in (u, ul @ [ou]) } |> Range
        | Empty -> Empty

    type T = { mutable Values: Values } 

    let create vs = { Values = vs } 
    let emptyVar () = { Values = Empty }

    let setVarValues (var: T) vs = var.Values <- vs
    let getVarValues { Values = vs } = vs 
    let updateVarValues f var = var |> getVarValues |> f |> setVarValues var

    let rangeToVc vs {Min = min; Max = max; Incr = incr; Unit = unit} =
        let u, ul = unit
        vs |> VC.create ul u

    let rangeToValues r =
        let {Min = min; Max = max; Incr = incr; Unit = unit} = r
        match VG.genValues min max incr None None with
        | vs when vs |> Seq.length > 0 -> 
            r |> rangeToVc vs |> Values
        | _ -> r |> Range

    let applyToRange f x = 
        match x with
        | Values _ -> x
        | Range r -> r |> f |> rangeToValues
        | Empty -> Empty

    let rangeToValue sv _ = sv
    let convRangeValue c sv r = 
        match sv with
        | Some v -> r |> rangeToVc [v] |> c |> List.head |> Some
        | None -> None
    let rangeToBaseValue = convRangeValue VC.getBaseValues
    let rangeToUnitValue = convRangeValue (VC.toUnitValueSet >> VS.toList)
    
    let setMin min = applyToRange (fun r -> { r with Min = min |> Some }) |> updateVarValues
    let setMax max = applyToRange (fun r -> { r with Max = max |> Some }) |> updateVarValues
    let setIncr incr = applyToRange (fun r -> { r with Incr = incr |> Some }) |> updateVarValues

    let setBaseMin min = 
        applyToRange (fun r -> { r with Min = r |> rangeToUnitValue (min |> Some) })
        |> updateVarValues
    let setBaseMax max = 
        applyToRange (fun r -> { r with Max = r |> rangeToUnitValue (max |> Some) })
        |> updateVarValues
    let setBaseIncr incr = 
        applyToRange (fun r -> { r with Incr = r |> rangeToUnitValue (incr |> Some) })
        |> updateVarValues

    let applyToValues f1 f2 f3 = function
        | Values vs -> vs |> f1
        | Range r -> r |> f2
        | Empty -> f3

    let getMin f1 f2 = 
        (fun r -> r |> f2 (match r.Min with | Some _ -> r.Min | None -> r.Incr)) |>
        applyToValues (fun vs -> vs |> f1 |> List.min |> Some) 
    let getMax f1 f2 = applyToValues (fun vs -> vs |> f1 |> List.max |> Some) (fun r -> r |> f2 r.Max) 
    let getIncr f = applyToValues (fun _ -> None) (fun r -> r |> f r.Incr) 
    let getValues f = applyToValues (fun vs -> vs |> f) (fun _ -> []) 

    let getUnitMin = getVarValues >> getMin VC.getUnitValues rangeToValue None
    let getUnitMax = getVarValues >> getMax VC.getUnitValues rangeToValue None
    let getUnitIncr = getVarValues >> getIncr rangeToValue None 
    let getUnitValues = getVarValues >> getValues VC.getUnitValues []

    let getBaseMin = getVarValues >> getMin VC.getBaseValues rangeToBaseValue None
    let getBaseMax = getVarValues >> getMax VC.getBaseValues rangeToBaseValue None
    let getBaseIncr = getVarValues >> getIncr rangeToBaseValue None
    let getBaseValues = getVarValues >> getValues VC.getBaseValues []

    let calc op vs1 vs2 = 
        match vs1, vs2 with
        | Values vc1, Values vc2 -> vc1 |> op <| vc2 |> Values 
        | Values vc,  Range r
        | Range r,    Values vc  -> (r |> rangeToVc []) |>  op <| vc |> Values
        | Range r1,   Range r2   -> (r1 |> rangeToVc []) |> op <| (r2 |> rangeToVc []) |> Values
        | _ -> Empty

    let mult = calc (*)
    let div = calc (/)
    let add = calc (+)
    let subtr = calc (-)

    let filterIncr (i: BigRational option) (v: BigRational) = 
        match i with
        | Some x -> (v.Numerator * x.Denominator) % (x.Numerator * v.Denominator) = 0I
        | None   -> v > 0N

    let filterMax m v =
        match m with
        | Some x -> v <= x
        | None -> true

    let filterMin m v =
        match m with
        | Some x -> v >= x
        | None -> true

    let setValues vs x =
        match x with
        | Values vs' -> vs' |> VC.setValues vs 
        | Range r ->
            let u, ul = r.Unit |> snd, r.Unit |> fst 
            vs 
            |> VC.convertToUnit ([] |> VC.create u ul)
            |> VC.getUnitValues
            |> List.filter(fun v -> v |> filterMin r.Min &&
                                    v |> filterMax r.Max &&
                                    v |> filterIncr r.Incr)
            |> VC.create u ul
        | Empty -> vs 
        |> Values

    let setValue v x =
        let toVc { Values = v } = 
            match v with
            | Values vs -> vs |> Some
            | Range r -> r |> rangeToVc [] |> Some
            | Empty -> None

        let vc = 
            match x |> toVc with
            | Some x' -> x' |> VC.withValueSet (v |> VS.create) |> Some
            | None -> None

        let values = 
            match vc with
            | Some vc' -> x.Values |> setValues vc'
            | None -> Empty

        match values with
        | Values vs when vs |> VC.getUnitValues |> List.length = 1 -> 
            x.Values <- values
        | _ -> ()

    let setResult y = function
        | Values vc when vc |> VC.getUnitValues |> Seq.length > 0 -> 
            if y.Values = Empty then printfn "%A" y
            let old = y.Values
            y.Values <- y.Values |> setValues vc 
            old = y.Values |> not
        | _ -> false

    let equals var1 var2 =
            match var1.Values, var2.Values with
            | Values vc1, Values vc2 -> vc1 |> VC.getUnitValues = (vc2 |> VC.getUnitValues)
            | Range r1, Range r2 -> r1 = r2
            | _ -> false
    
    let solve op y xs =
        let get { Values = v } = v
        match xs with
        | [] -> false
        | _ ->
            xs 
            |> List.map get
            |> List.reduce op
            |> setResult y

    let solveMult = solve mult 
    let solveDiv = solve div
    let solveAdd = solve add
    let solveSubtr = solve subtr

    let count = getUnitValues >> List.length
    let has n = count >> ((=) n)
    let hasValue = has 1
    let noValues = has 0

    type T with
        
        static member (*) (var1: T, var2: T) = mult var1.Values var2.Values
        static member (/) (var1: T, var2: T) = div var1.Values var2.Values

        static member (+) (var1: T, var2: T) = add var1.Values var2.Values
        static member (-) (var1: T, var2: T) = div var1.Values var2.Values

        static member (=!) (result, expr) = expr |> setResult result

        static member (==) (var1, var2) = var1 |> equals var2

        static member (=*) (y, xs) = xs |> solveMult y
        static member (=/) (y, xs) = xs |> solveDiv y
        static member (=+) (y, xs) = xs |> solveAdd y
        static member (=-) (y, xs) = xs |> solveSubtr y
     
                
module TestVariable =

    open Informedica.Utilities.Lists

    module VR = Variable
    module UN = Unit

    let test = [] |> VR.withUnit UN.Units.milliGram |> VR.create 

    test |> VR.getUnitIncr
    test |> VR.getUnitValues
    test |> VR.setIncr 1N
    test |> VR.setMax 10N
    test =! (test - test)
    test == test

    let freq = [1N..24N] |> VR.withUnit UN.Units.count |> VR.per 1N UN.Units.day |> VR.create 
    let tot = [] |> VR.withUnit UN.Units.milliGram |> VR.per 1N UN.Units.day |> VR.create 
    let tot2 = [] |> VR.withUnit UN.Units.milliGram |> VR.create 
    let qty = [10N] |> VR.withUnit UN.Units.milliGram |> VR.create  
    tot2 =* [qty]
    qty |> VR.setIncr 1N
    tot/ freq
    qty =/ [tot; freq]
    qty |> VR.getUnitValues |> Seq.length
    qty 
    let oneGram = [1N] |> VR.withUnit UN.Units.gram |> VR.create 
    let thousandMg = [1000N] |> VR.withUnit UN.Units.milliGram |> VR.create 
    oneGram + thousandMg

    let var1 = [] |> VR.withUnit UN.Units.milliLiter |> VR.create 
    let var2 = [10N] |> VR.withUnit UN.Units.milliLiter |> VR.create 
    let var3 = [10N] |> VR.withUnit UN.Units.milliLiter |> VR.create 
    let sum = [30N] |> VR.withUnit UN.Units.milliLiter |> VR.create 
    var1 =- [sum;var2;var3]
    var1 
       

module Equation =

    open Informedica.Utilities.Lists

    module VR = Variable
    
    type T =
        | ProductEquation of VR.T list
        | SumEquation of VR.T list
        | EmptyEquation

    let create c vars = vars |> c
    
    let createProductEq = create ProductEquation

    let createSumEq = create SumEquation

    let apply f1 f2 = function
        | ProductEquation vars -> vars |> f1
        | SumEquation vars -> vars |> f2
        | EmptyEquation -> [] |> f2

    let isSolved = List.forall VR.hasValue

    let solved = apply isSolved isSolved

    let getCount = List.map VR.count >> List.sum

    let count = apply getCount getCount

    let solveProductMinMax y x1 x2 =

        let set cmp op getf setf x = function
            | Some v1, Some v2 ->
                let r = v1 |> op <| v2
                match x |> getf with
                | Some v3 -> if cmp r v3 then x |> setf r
                | None -> x |> setf r
            | _ -> ()

        let setXmin = set (>) (/) VR.getBaseMin VR.setBaseMin        
        let setXmax = set (<) (/) VR.getBaseMax VR.setBaseMax

        let setYmin = set (>) (*) VR.getBaseMin VR.setBaseMin        
        let setYmax = set (<) (*) VR.getBaseMax VR.setBaseMax

        // Rule 1: x2.min = y.min / x1.max if x1.max = set && y.max / x1.max > x2.min || x2.min = not set 
        setXmin x1 (y |> VR.getBaseMin, x2 |> VR.getBaseMax)
        setXmin x2 (y |> VR.getBaseMin, x1 |> VR.getBaseMax)
        // Rule 2: x2.max = y.max / x1.min if x1.min = set and vice versa
        setXmax x1 (y |> VR.getBaseMax, x2 |> VR.getBaseMin)
        setXmax x2 (y |> VR.getBaseMax, x1 |> VR.getBaseMin)
        // Rule 3: y.min = Product(x.min) if all x.min = set
        setYmin y (x1 |> VR.getBaseMin, x2 |> VR.getBaseMin)
        // Rule 4: y.max = Product(x.max) if all x.max = set
        setYmax y (x2 |> VR.getBaseMax, x2 |> VR.getBaseMax)
        
        [y;x1;x2]

    let solveSumMinMax sum vars =
        
        let sumVar getf vars  =
            vars 
            |> List.map getf 
            |> List.filter Option.isSome
            |> List.map Option.get
            |> List.fold (+) 0N

        let set cmp getf setf v var =
            match v, var |> getf with
            | Some v', Some x -> if cmp v' x then var |> setf v'
            | Some v', None   -> if v' > 0N  then var |> setf v'
            | _ -> ()  
        
        let setMax = set (<) (VR.getBaseMax) (VR.setBaseMax)
        let setMin = set (>) (VR.getBaseMin) (VR.setBaseMin)
        
        // Rule 1: var.max = sum.max if var.max = not set || sum.max < var.max
        vars |> List.iter (setMax (sum |> VR.getBaseMax))

        // Rule 2: var.min = sum.min - Sum(var.min) if n - 1 var.min = set
        match vars |> List.filter(fun v -> v |> VR.getBaseMin |> Option.isNone) with
        | var::[] -> 
            let v = 
                match sum |> VR.getBaseMin with
                | None -> None
                | Some v -> v - (vars |> List.map VR.getBaseMin 
                                      |> List.filter Option.isSome 
                                      |> List.map Option.get
                                      |> List.sum) |> Some
            setMin v var
        | _ -> ()

        // Rule 3: sum.min = Sum(var.min) if Sum(var.min) > sum.min
        setMin (match sumVar VR.getBaseMin vars with | v when v > 0N -> v |> Some | _ -> None) sum
        // Rule 4: sum.max = Sum(var.max) if all var.max = set
        if vars |> List.map VR.getBaseMax |> List.exists Option.isNone then ()
            else
                setMax (match sumVar VR.getBaseMax vars with | v when v > 0N -> v |> Some | _ -> None) sum
        
        sum::vars

    let solveEquation op1 op2 vars =
        let c = vars |> List.length

        let rec calc c vars =
            let y, y', xs = match vars with | y::y'::xs -> y, y', xs | _ -> failwith "invalid equation"
            match c with
            | _ when vars |> List.length = c ->
                if op1 y (y'::xs) then true
                else y'::y::xs |> calc (c-1) 
            | _ when c > 0 ->
                if op2 y (y'::xs) then true
                else
                    (xs |> List.head)::y'::(xs |> List.tail)@[y] |> calc (c-1)
            | _ -> false

        calc c vars
        
    let solveProductEquation vars = 

        let solveq = solveEquation (=*) (=/) 

        match vars with
        | [y;x1;x2] ->
            let r = ref false
            while solveProductMinMax y x1 x2 |> solveq do
                r := true
            !r
        | _ -> failwith "Not a valid product equation"

    let solveSumEquation vars = 
        let solveq = solveEquation (=+) (=-) 

        if vars |> Seq.length <= 1 then false
        else
            match vars with
            | y::xs ->
                let r = ref false 
                while solveSumMinMax y xs |> solveq do
                    r := true
                !r
            | _ -> false

    type T with
        member this.Solve () = this |> apply solveProductEquation solveSumEquation
        member this.Solved = this |> solved
        member this.Length = this |> count


module Solver =
    
    module EQ = Equation

    let solve eqs =

        let filter = List.filter (EQ.solved >> not)
        let sort = List.sortBy EQ.count
       
        let rec calc (eqs: EQ.T list) =
            match eqs |> filter with
            | [] -> ()
            | e::rest -> 
                if e.Solve() then
                    e::rest |> sort |> calc
                else rest |> calc

        eqs |> sort |> calc


module Quantities =

    module VR = Variable
    module UN = Unit
    module EQ = Equation

    let (>>=) = Result.bindL
    let (<!>) = Result.liftR
    let (<*>) = Result.applyR

    let fluent f x = x |> f; x

    let create c = VR.create >> c

    type IQuantity =
        abstract member GetVariable: VR.T

    let apply f (qty: IQuantity) =
        qty.GetVariable |> f
    let applyFluent f x = fluent (apply f) x

    let setMin min = applyFluent (VR.setMin min) 
    let setMax max = applyFluent (VR.setMax max) 
    let setIncr incr = applyFluent (VR.setIncr incr) 
    let setValue v = applyFluent (VR.setValue v)         
        
    let getVar : (IQuantity -> VR.T) = apply id

    let set<'T when 'T :> IQuantity> (f: IQuantity -> IQuantity) (v: 'T) = 
     v :> IQuantity |> f :?> 'T


    let toProdEq eqs = eqs |> List.map getVar |> EQ.ProductEquation 
    let toSumEq  eqs = eqs |> List.map getVar |> EQ.SumEquation

    module Messages =

        type T =
            | NotATimeUnit of string
            | OnlyPostivesAllowed 
            | CouldNotFindUnit of string
            | CouldNotCreateUnit
            | CreatedUnit of string

        let map = function 
            | UN.Messages.CouldNotFindUnit u -> u |> CouldNotFindUnit
            | UN.Messages.CannotCreateAbbreviation _ -> CouldNotCreateUnit
            | UN.Messages.CannotCreateGroupName _ -> CouldNotCreateUnit
            | UN.Messages.CannotCreateName _ -> CouldNotCreateUnit
            | UN.Messages.CreatedAbbreviation a -> a |> CreatedUnit
            | UN.Messages.CreatedName n -> n |> CreatedUnit
            | UN.Messages.CreatedGroupName n -> n |> CreatedUnit
            | UN.Messages.MultiplierNotPositive _ -> CouldNotCreateUnit
            | UN.Messages.UnitIsRequired -> CouldNotCreateUnit


    module Validation = 

        let onlyPositiveNum ns x = 
            if ns |> List.forall ((<) 0N) then x |> Result.succeed
            else Messages.OnlyPostivesAllowed |> Result.fail 


    module Frequency =

        type T = Frequency of VR.T with
            interface IQuantity with 
                member this.GetVariable = let (Frequency v) = this in v

        let create vs q tu = 

            let cr u = 
                vs 
                |> VR.withUnit UN.Units.count 
                |> VR.per q u 
                |> create Frequency
        
            cr
            <!> UN.Units.timeFromString tu
            |> Result.mapMessagesR Messages.map
            >>= Validation.onlyPositiveNum vs
            >>= Validation.onlyPositiveNum [q]
            >>= (fun freq -> freq |> setIncr 1N |> Result.succeed)


        let freq v = create v
        let per v u f = f v u

        let empty () = VR.emptyVar () |> Frequency

        let setMin min = min |> setMin |> set<T>
        let setMax max = max |> setMax |>  set<T>
        let setIncr incr = incr |> setIncr |> set<T> 
        let setValue v = v |> setValue |> set<T>


    module Time = 

        type T = Time of VR.T with
            interface IQuantity with 
                member this.GetVariable = let (Time v) = this in v

        let create vs tu =
            let cr u = 
                vs 
                |> VR.withUnit u 
                |> create Time

            cr 
            <!> UN.Units.timeFromString tu
            |> Result.mapMessagesR Messages.map
            >>= Validation.onlyPositiveNum vs

        let empty () = VR.emptyVar () |> Time

        let setMin min = min |> setMin |> set<T>
        let setMax max = max |> setMax |>  set<T>
        let setIncr incr = incr |> setIncr |> set<T> 
        let setValue v = v |> setValue |> set<T>


    module Quantity =    
    
        type T = Quantity of VR.T with
            interface IQuantity with 
                member this.GetVariable = let (Quantity v) = this in v

        let create vs u =
            let cr u = 
                vs 
                |> VR.withUnit u 
                |> create Quantity
            cr 
            <!> UN.Units.createFromString u
            |> Result.mapMessagesR Messages.map
            >>= Validation.onlyPositiveNum vs


        let setMin min = min |> setMin |> set<T>
        let setMax max = max |> setMax |>  set<T>
        let setIncr incr = incr |> setIncr |> set<T> 
        let setValue v = v |> setValue |> set<T>
        let empty () = VR.emptyVar() |> Quantity

        module QuantityTests =
            
            open Swensen.Unquote

            // A quantity cannot be generate with an zero or negative numbers
            test <@ create [0N] "gram" |> Result.isFailure @>
            test <@ create [-1N] "gram" |> Result.isFailure @>

            // A quantity can be created with an empty list
            test <@ create [] "gram" |> Result.isSuccess @>

            // A quantity cannot be created with a null or empty unit string
            test <@ create [] "" |> Result.isFailure @>
            test <@create [] null |> Result.isFailure @>

    module Total =

        type T = Total of VR.T with
            interface IQuantity with 
                member this.GetVariable = let (Total v) = this in v

        let create vs u1 tq u2 =
            let cr u1 u2 = 
                vs 
                |> VR.withUnit u1 
                |> VR.per tq u2 
                |> create Total
        
            cr 
            <!> UN.Units.createFromString u1
            <*> UN.Units.timeFromString u2
            |> Result.mapMessagesR Messages.map
            >>= Validation.onlyPositiveNum vs
            >>= Validation.onlyPositiveNum [tq]

        let total vs u1 = create vs u1
        let per tq tu f = f tq tu

        let empty () = VR.emptyVar() |> Total

        let setMin min = min |> setMin |> set<T>
        let setMax max = max |> setMax |>  set<T>
        let setIncr incr = incr |> setIncr |> set<T> 
        let setValue v = v |> setValue |> set<T>


    module Concentration =

        type T = Concentration of VR.T with
            interface IQuantity with 
                member this.GetVariable = let (Concentration v) = this in v

        let create vs u1 u2 =
            let cr u1 u2 = 
                vs 
                |> VR.withUnit u1 
                |> VR.per 1N u2 
                |> create Concentration
        
            cr 
            <!> UN.Units.createFromString u1
            <*> UN.Units.createFromString u2
            |> Result.mapMessagesR Messages.map
            >>= Validation.onlyPositiveNum vs

        let conc v = create v
        let per v u f = f v u

        let empty () = VR.emptyVar() |> Concentration

        let setMin min = min |> setMin |> set<T>
        let setMax max = max |> setMax |>  set<T>
        let setIncr incr = incr |> setIncr |> set<T> 
        let setValue v = v |> setValue |> set<T>


    module QuantityAdjust =

        type T = QuantityAdjust of VR.T with
            interface IQuantity with 
                member this.GetVariable = let (QuantityAdjust v) = this in v 

        let create vs u v au = 
            let cr u a = 
                vs 
                |> VR.withUnit u
                |> VR.per v a
                |> create QuantityAdjust

            cr 
            <!> UN.Units.fromString u
            <*> UN.Units.adjustFromString au
            |> Result.mapMessagesR Messages.map
            >>= Validation.onlyPositiveNum vs
            >>= Validation.onlyPositiveNum [v]

        let quantity v = create v
        let adjustBy v u f = f v u

        let empty () = VR.emptyVar() |> QuantityAdjust

        let setMin min = min |> setMin |> set<T>
        let setMax max = max |> setMax |>  set<T>
        let setIncr incr = incr |> setIncr |> set<T> 
        let setValue v = v |> setValue |> set<T>


    module TotalAdjust =

        type T = TotalAdjust of VR.T with
            interface IQuantity with 
                member this.GetVariable = let (TotalAdjust v) = this in v

        let create vs u av au tv tu = 
            let cr u au tu = 
                vs 
                |> VR.withUnit u
                |> VR.per av au
                |> VR.per tv tu
                |> create TotalAdjust

            cr 
            <!> UN.Units.fromString u
            <*> UN.Units.adjustFromString au  
            <*> UN.Units.timeFromString tu 
            |> Result.mapMessagesR Messages.map
            >>= Validation.onlyPositiveNum vs
            >>= Validation.onlyPositiveNum [av]
            >>= Validation.onlyPositiveNum [tv]

        let doseTot v = create v

        let total v = create v
        let adjustBy v u f = f v u
        let per v u f = f v u

        let empty () = VR.emptyVar() |> TotalAdjust

        let setMin min = min |> setMin |> set<T>
        let setMax max = max |> setMax |>  set<T>
        let setIncr incr = incr |> setIncr |> set<T> 
        let setValue v = v |> setValue |> set<T>


    module Rate =

        type T = Rate of VR.T with
            interface IQuantity with 
                member this.GetVariable = let (Rate v) = this in v

        let create vs u v tu = 
            let cr u tu = 
                vs 
                |> VR.withUnit u
                |> VR.per v tu
                |> create Rate

            cr 
            <!> UN.Units.fromString u
            <*> UN.Units.timeFromString tu
            |> Result.mapMessagesR Messages.map
            >>= Validation.onlyPositiveNum vs
            >>= Validation.onlyPositiveNum [v]

        let rate v = create v
        let per v u f = f v u

        let empty () = VR.emptyVar() |> Rate

        let setMin min = min |> setMin |> set<T>
        let setMax max = max |> setMax |>  set<T>
        let setIncr incr = incr |> setIncr |> set<T> 
        let setValue v = v |> setValue |> set<T>


    module RateAdjust =

        type T = RateAdjust of VR.T with
            interface IQuantity with 
                member this.GetVariable = let (RateAdjust v) = this in v

        let create vs u av au tv tu = 
            let cr u au tu = 
                vs 
                |> VR.withUnit u
                |> VR.per av au
                |> VR.per tv tu
                |> create RateAdjust

            cr 
            <!> UN.Units.fromString u
            <*> UN.Units.adjustFromString au
            <*> UN.Units.timeFromString tu
            |> Result.mapMessagesR Messages.map
            >>= Validation.onlyPositiveNum vs
            >>= Validation.onlyPositiveNum [av]
            >>= Validation.onlyPositiveNum [tv]


        let doseRate v = create v

        let doseTotAdj v = create v
        let adjustBy v u (f: BigRational -> string -> BigRational -> string -> T Option) = f v u

        let per v u (f: BigRational -> string -> T Option) = f v u

        let empty () = VR.emptyVar() |> RateAdjust

        let setMin min = min |> setMin |> set<T>
        let setMax max = max |> setMax |>  set<T>
        let setIncr incr = incr |> setIncr |> set<T> 
        let setValue v = v |> setValue |> set<T>


module Product =

    module VR = Variable
    module QT = Quantities
    module RW = Railway
    module WS = WrappedString

    let (>>=) = Result.bindL
    let (<!>) = Result.liftR
    let (<*>) = Result.applyR

    let mapToTuple = RW.mapToTuple
    let mapToTriple = RW.mapToTriple

    let set x = fun _ -> x 

    module Messages = 

        type T = 
            | CreatedGenericName of string
            | CannotCreateGenericName of string
            | SubstanceConcentrationShouldBeGreaterThan0
            | CannotCreateQuantity of string
            | CreatedUnit of string

    module Substance = 

        module Dto =
             
            type T = Informedica.GenPres.Contracts.Data.ProductSubstance

            let create () = new T()

        module GenericName =

            type T = GenericName of WS.StringMinMax.T

            let create s = 

                let map = function
                    | WS.Messages.CreatedString _ -> 
                        Messages.CreatedGenericName s
                    | WS.Messages.StringIsEmpty -> 
                        Messages.CannotCreateGenericName "empty string"
                    | WS.Messages.StringIsNull -> 
                        Messages.CannotCreateGenericName "null value"
                    | WS.Messages.StringLargerThan n -> 
                        Messages.CannotCreateGenericName ("string larger than " + string n)
                    | WS.Messages.StringSmallerThan n -> 
                        Messages.CannotCreateGenericName ("string is smaller than " + string n)

                let toLower s = s |> WS.StringMinMax.apply StringBCL.toLower |> Result.succeed

                s 
                |> WS.StringMinMax.create 2 30
                >>= toLower
                |> Result.mapMessagesR map
                >>= (GenericName >> Result.succeed)

            let apply f (GenericName n) = n |> WS.apply f

            let get = apply id
            let change f x = x |> apply f |> create
                


        module GenericNameTests =

            open Swensen.Unquote
            
            // A generic name cannot be created with an empty string
            test <@ GenericName.create "" |> Result.isFailure @>

            // A generic name cannot be created with a null string
            test<@  GenericName.create null |> Result.isFailure @>

            // A generic name cannot be created with a string smaller than 2 characters
            test<@ GenericName.create "1" |> Result.isFailure @>
            test<@ GenericName.create "12" |> Result.isSuccess @>

            // A generic name cannot be created with a string larger than 30 characters
            test<@  [ for _ in [1..31] do yield "s" ] |> List.fold (+) "" |> GenericName.create |> Result.isFailure @>
            test<@  [ for _ in [1..30] do yield "s" ] |> List.fold (+) "" |> GenericName.create |> Result.isSuccess @>

            // A generic name is all lower case
            test<@ GenericName.create "LOWERCASE" 
                   |> Result.either (fun (gn, _) -> gn |> GenericName.get = "lowercase")
                                    (fun _ -> false)  @>

        type T =
            { 
                Name: GenericName.T
                Quantity: QT.Quantity.T
                Concentration: QT.Concentration.T 
            } 

        let apply f (x: T) = x |> f

        let create name qty conc = 
                { 
                    Name = name 
                    Quantity = qty
                    Concentration = conc
                } 

        let dtoToSubstance (dto: Dto.T) =

            let map = function 
            | QT.Messages.CouldNotCreateUnit -> Messages.CannotCreateQuantity "cannot create unit"
            | QT.Messages.CouldNotFindUnit s -> Messages.CannotCreateQuantity ("cannot find unit: " + s)
            | QT.Messages.NotATimeUnit s -> Messages.CannotCreateQuantity ("unit " + s + " is not a time unit")
            | QT.Messages.OnlyPostivesAllowed -> Messages.CannotCreateQuantity "negative or zero numbers"
            | QT.Messages.CreatedUnit u -> u |> Messages.CreatedUnit

            let conc = dto.Concentration |> Informedica.Utilities.Math.floatToBigN

            let quantityOrFail = 
                QT.Quantity.create [] dto.Unit
                |> Result.mapMessagesR map

            let concOrFail =
                QT.Concentration.create [conc] dto.Unit dto.Adjust
                |> Result.mapMessagesR map

            create
            <!> GenericName.create dto.Name
            <*> quantityOrFail
            <*> concOrFail

        let getName = apply (fun s -> s.Name)
        let getQuantity = apply (fun s -> s.Quantity)
        let getConcentration = apply (fun s -> s.Concentration)

        let applyQuantity k = apply (fun s -> { s with Quantity = s.Quantity |> k })
        let applyConcentration k = apply (fun s -> { s with Concentration = s.Concentration |> k })

        let setQuantity x = x |> set |> applyQuantity
        let setConcentration = set >> applyConcentration

        let setQuantityValue x = x |>  QT.Quantity.setValue |> applyQuantity
        let setConcentrationValue x = x |> QT.Concentration.setValue |> applyConcentration

        let setQuantityIncr x = x |> QT.Quantity.setIncr |> applyQuantity
        let setConcentrationIncr x = x |> QT.Concentration.setIncr |> applyConcentration

        let setQuantityMin x = x |> QT.Quantity.setMin |> applyQuantity
        let setConcentrationMin x = x |> QT.Concentration.setMin |> applyConcentration

        let setQuantityMax x = x |> QT.Quantity.setMax |> applyQuantity
        let setConcentrationMax x = x |> QT.Concentration.setMax |> applyConcentration


    type T = 
        { 
            Name: string; 
            Quantity: QT.Quantity.T 
            Substances: Substance.T * Substance.T list
        } 
         
    let apply f (x: T) = x |> f

    let create n qty subst substs = 
            { 
                Name = n 
                Quantity = qty
                Substances = subst, substs
            } 


    let getName = apply (fun s -> s.Name)
    let getQuantity = apply (fun s -> s.Quantity)

    let applyQuantity k = apply (fun s -> { s with Quantity = s.Quantity |> k })

    let setQuantity x = x |> set |> applyQuantity 

    let setQuantityValue x = x |>  QT.Quantity.setValue |> applyQuantity 

    let setQuantityIncr x = x |> QT.setIncr |> applyQuantity

    let setQuantityMin x = x |> QT.setMin |> applyQuantity

    let setQuantityMax x = x |> QT.setMax |> applyQuantity

    let createProduct name vs un sq sun = 
        let crtSubst = Substance.createSubst name sq sun vs
        let crt (qty, subst) = 
            create name qty subst [] |> RW.succeed

        QT.Quantity.create vs un
        |> mapToTuple crtSubst sun
        >>= crt
        
 
module Prescription =
    
    module WS = WrappedString
    module VR = Variable
    module UN = Unit
    module QT = Quantities
    module EQ = Equation


    let (>>=) m k = Option.bind k m
    let lift f x = x |> f |> Some
    let liftp p m = if p then m |> Some else None

    type T =
        | PRN of Prescription
        | NonPRN of Prescription 
    and Prescription = 
        | Process
        | Continuous
        | Discontinuous of QT.Frequency.T
        | Timed of QT.Frequency.T * QT.Time.T

    let getPrescription = function | PRN p | NonPRN p -> p

    let createProcess = Process |> Some

    let createContinuous () = Continuous

    let createDiscontinous f = f |> Discontinuous

    let createTimed f t =  (f, t) |> Timed

    let toPRN = PRN
    let toNonPRN = NonPRN
    
    let apply f = function | PRN p | NonPRN p -> p |> f


module Orderable =

    module UN = Unit
    module VR = Variable
    module EQ = Equation
    module SV = Solver
    module QT = Quantities
    module PN = Prescription

    let (>>=) m k = Option.bind k m
    
    let set x = fun _ -> x

    type Name = Name of string

    module Dose = 

        module Adjust =

            type T = 
            | Weight of QT.Quantity.T
            | Surface of QT.Quantity.T
            | Length of QT.Quantity.T
            | NoAdjust

            let createWeightAdjust = Weight
            let createSurfaceAdjust = Surface
            let createLengthAdjust = Length
            let createNoAdjust = NoAdjust

        type T = 
            {
                AdminQuantity: QT.Quantity.T 
                AdminTotal: QT.Total.T 
                AdminRate: QT.Rate.T 
                DoseQuantity: QT.QuantityAdjust.T 
                DoseTotal: QT.TotalAdjust.T 
                DoseRate: QT.RateAdjust.T                     
            }

        let create qt tot rate dqt dtot drate =
            {
                AdminQuantity = qt 
                AdminTotal = tot 
                AdminRate = rate 
                DoseQuantity = dqt 
                DoseTotal = dtot 
                DoseRate = drate                     
            } 

        let apply f (x: T) = x |> f

        let getAdminQuantity = apply (fun d -> d.AdminQuantity)
        let getAdminTotal = apply (fun d -> d.AdminTotal)
        let getAdminRate = apply (fun d -> d.AdminRate)
        let getDoseQuantity = apply (fun d -> d.DoseQuantity)
        let getDoseTotal = apply (fun d -> d.DoseTotal)
        let getDoseRate = apply (fun d -> d.DoseRate)

        let applyAdminQuantity k = apply (fun dose -> { dose with AdminQuantity = dose.AdminQuantity |> k })
        let applyAdminTotal k = apply (fun dose -> { dose with AdminTotal = dose.AdminTotal |> k })
        let applyAdminRate k = apply (fun dose -> { dose with AdminRate = dose.AdminRate |> k })
        let applyDoseQuantity k = apply (fun dose -> { dose with DoseQuantity = dose.DoseQuantity |> k })
        let applyDoseTotal k = apply (fun dose -> { dose with DoseTotal = dose.DoseTotal |> k })
        let applyDoseRate k = apply (fun dose -> { dose with DoseRate = dose.DoseRate |> k })

        let setAdminQuantity x = x |> set |> applyAdminQuantity
        let setAdminTotal x = x |> set |> applyAdminTotal
        let setAdminRate x = x |> set |> applyAdminRate
        let setDoseQuantity x = x |> set |> applyDoseQuantity
        let setDoseTotal x = x |> set |> applyDoseTotal
        let setDoseRate x = x |> set |> applyDoseRate

        let setAdminQuantityValue x = x |> QT.Quantity.setValue |> applyAdminQuantity
        let setAdminTotalValue x = x |> QT.Total.setValue |> applyAdminTotal
        let setAdminRateValue x = x |> QT.Rate.setValue |> applyAdminRate
        let setDoseQuantityValue x = x |> QT.QuantityAdjust.setValue |> applyDoseQuantity
        let setDoseTotalValue x = x |> QT.TotalAdjust.setValue |> applyDoseTotal
        let setDoseRateValue x = x |> QT.RateAdjust.setValue |> applyDoseRate

        let setAdminQuantityIncr x = x |> QT.Quantity.setIncr |> applyAdminQuantity
        let setAdminTotalIncr x = x |> QT.Total.setIncr |> applyAdminTotal
        let setAdminRateIncr x = x |> QT.Rate.setIncr |> applyAdminRate
        let setDoseQuantityIncr x = x |> QT.QuantityAdjust.setIncr |> applyDoseQuantity
        let setDoseTotalIncr x = x |> QT.TotalAdjust.setIncr |> applyDoseTotal
        let setDoseRateIncr x = x |> QT.RateAdjust.setIncr |> applyDoseRate

        let setAdminQuantityMin x = x |> QT.Quantity.setMin |> applyAdminQuantity
        let setAdminTotalMin x = x |> QT.Total.setMin |> applyAdminTotal
        let setAdminRateMin x = x |> QT.Rate.setMin |> applyAdminRate
        let setDoseQuantityMin x = x |> QT.QuantityAdjust.setMin |> applyDoseQuantity
        let setDoseTotalMin x = x |> QT.TotalAdjust.setMin |> applyDoseTotal
        let setDoseRateMin x = x |> QT.RateAdjust.setMin |> applyDoseRate

        let setAdminQuantityMax x = x |> QT.Quantity.setMax |> applyAdminQuantity
        let setAdminTotalMax x = x |> QT.Total.setMax |> applyAdminTotal
        let setAdminRateMax x = x |> QT.Rate.setMax |> applyAdminRate
        let setDoseQuantityMax x = x |> QT.QuantityAdjust.setMax |> applyDoseQuantity
        let setDoseTotalMax x = x |> QT.TotalAdjust.setMax |> applyDoseTotal
        let setDoseRateMax x = x |> QT.RateAdjust.setMax |> applyDoseRate
    
    module Drug =

        module PR = Product

        

        module Substance = 

            type T =
                { 
                    Substance: PR.Substance.T
                    Quantity: QT.Quantity.T // Is the substance drug quantity
                    Concentration: QT.Concentration.T // Is the substance drug concentration
                    Dose: Dose.T
                } 

            let apply f (x: T) = x |> f

            let create subst dose qt conc = 
                    { 
                        Substance = subst 
                        Quantity = qt
                        Concentration = conc
                        Dose = dose
                    } 

            let getName = apply (fun s -> s.Substance.Name)
            let getDrugQuantity = apply (fun s -> s.Quantity)
            let getComponentQuantity = apply (fun s -> s.Substance.Quantity)
            let getComponentConcentration = apply (fun s -> s.Substance.Concentration)
            let getDrugConcentration = apply (fun s -> s.Concentration)
            let getAdminQuantity = apply (fun s -> s.Dose.AdminQuantity)
            let getAdminTotal = apply (fun s -> s.Dose.AdminTotal)
            let getAdminRate = apply (fun s -> s.Dose.AdminRate)
            let getDoseQuantity = apply (fun s -> s.Dose.DoseQuantity)
            let getDoseTotal = apply (fun s -> s.Dose.DoseTotal)
            let getDoseRate = apply (fun s -> s.Dose.DoseRate)

            let applySubstance k = apply (fun s -> { s with Substance = s.Substance |> k })
            let applyDose k = apply (fun s -> { s with Dose = s.Dose |> k })
            let applyQuantity k = apply (fun s -> { s with Quantity = s.Quantity |> k })
            let applyConcentration k = apply (fun s -> { s with Concentration = s.Concentration |> k })

            let setDrugQuantity x = x |> set |> applyQuantity
            let setDrugConcentration x = x |> set |> applyConcentration
            let setAdminQuantity x = x |> Dose.setAdminQuantity |> applyDose
            let setAdminTotal x = x |> Dose.setAdminTotal |> applyDose
            let setAdminRate x = x |> Dose.setAdminRate |> applyDose
            let setDoseQuantity x = x |> Dose.setDoseQuantity |> applyDose
            let setDoseTotal x = x |> Dose.setDoseTotal |> applyDose
            let setDoseRate x = x |> Dose.setDoseRate |> applyDose

            let setComponentQuantityValue x = x |> PR.Substance.setQuantityValue |> applySubstance
            let setComponentConcentrationValue x = x |> PR.Substance.setConcentrationValue |> applySubstance
            let setDrugQuantityValue x = x |> QT.Quantity.setValue |> applyQuantity
            let setDrugConcentrationValue x = x |> QT.Concentration.setValue |> applyConcentration
            let setAdminQuantityValue x = x |> Dose.setAdminQuantityValue |> applyDose
            let setAdminTotalValue x = x |> Dose.setAdminTotalValue |> applyDose
            let setAdminRateValue x = x |> Dose.setAdminRateValue |> applyDose
            let setDoseQuantityValue x = x |> Dose.setDoseQuantityValue |> applyDose
            let setDoseTotalValue x = x |> Dose.setDoseTotalValue |> applyDose
            let setDoseRateValue x = x |> Dose.setDoseRateValue |> applyDose

            let setComponentQuantityIncr x = x |> PR.Substance.setQuantityIncr |> applySubstance
            let setComponentConcentrationIncr x = x |> PR.Substance.setConcentrationIncr |> applySubstance
            let setDrugQuantityIncr x = x |> QT.Quantity.setIncr |> applyQuantity
            let setDrugConcentrationIncr x = x |> QT.Concentration.setIncr |> applyConcentration
            let setAdminQuantityIncr x = x |> Dose.setAdminQuantityIncr |> applyDose
            let setAdminTotalIncr x = x |> Dose.setAdminTotalIncr |> applyDose
            let setAdminRateIncr x = x |> Dose.setAdminRateIncr |> applyDose
            let setDoseQuantityIncr x = x |> Dose.setDoseQuantityIncr |> applyDose
            let setDoseTotalIncr x = x |> Dose.setDoseTotalIncr |> applyDose
            let setDoseRateIncr x = x |> Dose.setDoseRateIncr |> applyDose

            let setComponentQuantityMin x = x |> PR.Substance.setQuantityMin |> applySubstance
            let setComponentConcentrationMin x = x |> PR.Substance.setConcentrationMin |> applySubstance
            let setDrugQuantityMin x = x |> QT.Quantity.setMin |> applyQuantity
            let setDrugConcentrationMin x = x |> QT.Concentration.setMin |> applyConcentration
            let setAdminQuantityMin x = x |> Dose.setAdminQuantityMin |> applyDose
            let setAdminTotalMin x = x |> Dose.setAdminTotalMin |> applyDose
            let setAdminRateMin x = x |> Dose.setAdminRateMin |> applyDose
            let setDoseQuantityMin x = x |> Dose.setDoseQuantityMin |> applyDose
            let setDoseTotalMin x = x |> Dose.setDoseTotalMin |> applyDose
            let setDoseRateMin x = x |> Dose.setDoseRateMin |> applyDose

            let setComponentQuantityMax x = x |> PR.Substance.setQuantityMax |> applySubstance
            let setComponentConcentrationMax x = x |> PR.Substance.setConcentrationMax |> applySubstance
            let setDrugQuantityMax x = x |> QT.Quantity.setMax |> applyQuantity
            let setDrugConcentrationMax x = x |> QT.Concentration.setMax |> applyConcentration
            let setAdminQuantityMax x = x |> Dose.setAdminQuantityMax |> applyDose
            let setAdminTotalMax x = x |> Dose.setAdminTotalMax |> applyDose
            let setAdminRateMax x = x |> Dose.setAdminRateMax |> applyDose
            let setDoseQuantityMax x = x |> Dose.setDoseQuantityMax |> applyDose
            let setDoseTotalMax x = x |> Dose.setDoseTotalMax |> applyDose
            let setDoseRateMax x = x |> Dose.setDoseRateMax |> applyDose


        module Component =

            type T = 
                { 
                    Product: PR.T
                    Quantity: QT.Quantity.T 
                    Concentration: QT.Concentration.T 
                    Dose: Dose.T
                    Substances: Substance.T * Substance.T list
                } 

            let apply f (x: T) = x |> f

            let create p qt conc d = 
                // Note: not correct yet!
                let createS s = Substance.create s d qt conc
                { 
                    Product = p
                    Quantity = qt
                    Concentration = conc
                    Dose = d
                    Substances = 
                        (p.Substances |> fst |> createS, 
                            p.Substances |> snd |> List.map createS)
                } 


            let getName = apply (fun c -> c.Product.Name)
            let getQuantity = apply (fun c -> c.Quantity)
            let getTotal = apply (fun c -> c.Product.Quantity)
            let getConcentration = apply (fun c -> c.Concentration)
            let getAdminQuantity = apply (fun c -> c.Dose.AdminQuantity)
            let getAdminTotal = apply (fun c -> c.Dose.AdminTotal)
            let getAdminRate = apply (fun c -> c.Dose.AdminRate)
            let getDoseQuantity = apply (fun c -> c.Dose.DoseQuantity)
            let getDoseTotal = apply (fun c -> c.Dose.DoseTotal)
            let getDoseRate = apply (fun c -> c.Dose.DoseRate)

            let applyProduct k = apply (fun c -> { c with Product = c.Product |> k })
            let applyDose k = apply (fun c -> { c with Dose = c.Dose |> k })
            let applyQuantity k = apply (fun c -> { c with Quantity = c.Quantity |> k })
            let applyConcentration k = apply (fun c -> { c with Concentration = c.Concentration |> k })

            let setQuantity x = x |> set |> applyQuantity
            let setConcentration x = x |> set|> applyConcentration
            let setAdminQuantity x = x |> Dose.setAdminQuantity |> applyDose
            let setAdminTotal x = x |> Dose.setAdminTotal |> applyDose
            let setAdminRate x = x |> Dose.setAdminRate |> applyDose
            let setDoseQuantity x = x |> Dose.setDoseQuantity |> applyDose
            let setDoseTotal x = x |> Dose.setDoseTotal |> applyDose
            let setDoseRate x = x |> Dose.setDoseRate |> applyDose

            let setQuantityValue x = x |> QT.Quantity.setValue |> applyQuantity
            let setConcentrationValue x = x |> QT.Concentration.setValue |> applyConcentration
            let setAdminQuantityValue x = x |> Dose.setAdminQuantityValue |> applyDose
            let setAdminTotalValue x = x |> Dose.setAdminTotalValue |> applyDose
            let setAdminRateValue x = x |> Dose.setAdminRateValue |> applyDose
            let setDoseQuantityValue x = x |> Dose.setDoseQuantityValue |> applyDose
            let setDoseTotalValue x = x |> Dose.setDoseTotalValue |> applyDose
            let setDoseRateValue x = x |> Dose.setDoseRateValue |> applyDose

            let setQuantityIncr x = x |> QT.Quantity.setIncr |> applyQuantity
            let setConcentrationIncr x = x |> QT.Concentration.setIncr |> applyConcentration
            let setAdminQuantityIncr x = x |> Dose.setAdminQuantityIncr |> applyDose
            let setAdminTotalIncr x = x |> Dose.setAdminTotalIncr |> applyDose
            let setAdminRateIncr x = x |> Dose.setAdminRateIncr |> applyDose
            let setDoseQuantityIncr x = x |> Dose.setDoseQuantityIncr |> applyDose
            let setDoseTotalIncr x = x |> Dose.setDoseTotalIncr |> applyDose
            let setDoseRateIncr x = x |> Dose.setDoseRateIncr |> applyDose

            let setQuantityMin x = x |> QT.Quantity.setMin |> applyQuantity
            let setConcentrationMin x = x |> QT.Concentration.setMin |> applyConcentration
            let setAdminQuantityMin x = x |> Dose.setAdminQuantityMin |> applyDose
            let setAdminTotalMin x = x |> Dose.setAdminTotalMin |> applyDose
            let setAdminRateMin x = x |> Dose.setAdminRateMin |> applyDose
            let setDoseQuantityMin x = x |> Dose.setDoseQuantityMin |> applyDose
            let setDoseTotalMin x = x |> Dose.setDoseTotalMin |> applyDose
            let setDoseRateMin x = x |> Dose.setDoseRateMin |> applyDose

            let setQuantityMax x = x |> QT.Quantity.setMax |> applyQuantity
            let setConcentrationMax x = x |> QT.Concentration.setMax |> applyConcentration
            let setAdminQuantityMax x = x |> Dose.setAdminQuantityMax |> applyDose
            let setAdminTotalMax x = x |> Dose.setAdminTotalMax |> applyDose
            let setAdminRateMax x = x |> Dose.setAdminRateMax |> applyDose
            let setDoseQuantityMax x = x |> Dose.setDoseQuantityMax |> applyDose
            let setDoseTotalMax x = x |> Dose.setDoseTotalMax |> applyDose
            let setDoseRateMax x = x |> Dose.setDoseRateMax |> applyDose


        type T = 
            { 
                Name: string; 
                Total: QT.Quantity.T
                Dose: Dose.T
                Components: Component.T * Component.T list
            } 

        let apply f (x: T) = x |> f

        let getComps (d: T) = (d.Components |> fst)::(d.Components |> snd)
        let getSubs (c: Component.T) = (c.Substances |> fst)::(c.Substances |> snd)


        let getName = apply (fun d -> d.Name)
        let getTotal = apply (fun d -> d.Total)
        let getAdminQuantity = apply (fun d -> d.Dose.AdminQuantity)
        let getAdminTotal = apply (fun d -> d.Dose.AdminTotal)
        let getAdminRate = apply (fun d -> d.Dose.AdminRate)
        let getDoseQuantity = apply (fun d -> d.Dose.DoseQuantity)
        let getDoseTotal = apply (fun d -> d.Dose.DoseTotal)
        let getDoseRate = apply (fun d -> d.Dose.DoseRate)

        let applyDose k = apply (fun d -> { d with Dose = d.Dose |> k })
        let applyTotal k = apply (fun d -> { d with Total = d.Total |> k })

        let setTotal x = x |> set |> applyTotal
        let setAdminQuantity x = x |> Dose.setAdminQuantity |> applyDose
        let setAdminTotal x = x |> Dose.setAdminTotal |> applyDose
        let setAdminRate x = x |> Dose.setAdminRate |> applyDose
        let setDoseQuantity x = x |> Dose.setDoseQuantity |> applyDose
        let setDoseTotal x = x |> Dose.setDoseTotal |> applyDose
        let setDoseRate x = x |> Dose.setDoseRate |> applyDose

        let setTotalValue x = x |> QT.Quantity.setValue |> applyTotal
        let setAdminQuantityValue x = x |> Dose.setAdminQuantityValue |> applyDose
        let setAdminTotalValue x = x |> Dose.setAdminTotalValue |> applyDose
        let setAdminRateValue x = x |> Dose.setAdminRateValue |> applyDose
        let setDoseQuantityValue x = x |> Dose.setDoseQuantityValue |> applyDose
        let setDoseTotalValue x = x |> Dose.setDoseTotalValue |> applyDose
        let setDoseRateValue x = x |> Dose.setDoseRateValue |> applyDose

        let setTotalIncr x = x |> QT.Quantity.setIncr |> applyTotal
        let setAdminQuantityIncr x = x |> Dose.setAdminQuantityIncr |> applyDose
        let setAdminTotalIncr x = x |> Dose.setAdminTotalIncr |> applyDose
        let setAdminRateIncr x = x |> Dose.setAdminRateIncr |> applyDose
        let setDoseQuantityIncr x = x |> Dose.setDoseQuantityIncr |> applyDose
        let setDoseTotalIncr x = x |> Dose.setDoseTotalIncr |> applyDose
        let setDoseRateIncr x = x |> Dose.setDoseRateIncr |> applyDose

        let setTotalMin x = x |> QT.Quantity.setMin |> applyTotal
        let setAdminQuantityMin x = x |> Dose.setAdminQuantityMin |> applyDose
        let setAdminTotalMin x = x |> Dose.setAdminTotalMin |> applyDose
        let setAdminRateMin x = x |> Dose.setAdminRateMin |> applyDose
        let setDoseQuantityMin x = x |> Dose.setDoseQuantityMin |> applyDose
        let setDoseTotalMin x = x |> Dose.setDoseTotalMin |> applyDose
        let setDoseRateMin x = x |> Dose.setDoseRateMin |> applyDose

        let setTotalMax x = x |> QT.Quantity.setMax |> applyTotal
        let setAdminQuantityMax x = x |> Dose.setAdminQuantityMax |> applyDose
        let setAdminTotalMax x = x |> Dose.setAdminTotalMax |> applyDose
        let setAdminRateMax x = x |> Dose.setAdminRateMax |> applyDose
        let setDoseQuantityMax x = x |> Dose.setDoseQuantityMax |> applyDose
        let setDoseTotalMax x = x |> Dose.setDoseTotalMax |> applyDose
        let setDoseRateMax x = x |> Dose.setDoseRateMax |> applyDose


        let toProdEqs qts = qts |> List.map QT.toProdEq
        let toSumEqs  qts = qts |> List.map QT.toSumEq


 //     substAdminRate = adjust x substDoseRate 
 //     substDoseRate  = substDrugConc x drugDoseRate
 //     substDoseRate  = substCompConc x compDoseRate
 //     substAdminRate = substDrugConc x drugAdminRate
 //     substAdminRate = substCompConc x compAdminRate
        let contSubstEqs adj drug comp eqs subst =

            let drugAdminRate  = drug |> getAdminRate :> QT.IQuantity
            let drugDoseRate   = drug |> getDoseRate :> QT.IQuantity
            let compAdminRate  = comp |> Component.getAdminRate :> QT.IQuantity
            let compDoseRate   = comp |> Component.getDoseRate :> QT.IQuantity
            let substAdminRate = subst |> Substance.getAdminRate :> QT.IQuantity
            let substDoseRate  = subst |> Substance.getDoseRate :> QT.IQuantity
            let substDrugConc  = subst |> Substance.getDrugConcentration :> QT.IQuantity
            let substCompConc  = subst |> Substance.getComponentConcentration :> QT.IQuantity
            
            let adjust adj eqs = 
                match adj with
                | Dose.Adjust.Weight a 
                | Dose.Adjust.Length a
                | Dose.Adjust.Surface a ->
                    let adjust = a :> QT.IQuantity
                    [[substAdminRate; adjust; substDoseRate]] 
                    |> toProdEqs 
                    |> List.append eqs
                | Dose.Adjust.NoAdjust -> eqs

            [[substDoseRate;  substDrugConc; drugDoseRate]
             [substDoseRate;  substCompConc; compDoseRate]
             [substAdminRate; substDrugConc; drugAdminRate]
             [substAdminRate; substCompConc; compAdminRate]] 
            |> toProdEqs
            |> List.append eqs

//      compAdminRate = adjust x compDoseRate
//      compAdminRate = compConc x drugAdminRate
//      compDoseRate  = compConc x drugDoseRate
        let contCompEqs adj drug eqs comp = 

            let drugAdminRate = drug |> getAdminRate :> QT.IQuantity
            let drugDoseRate = drug |> getDoseRate :> QT.IQuantity
            let compAdminRate = comp |> Component.getAdminRate :> QT.IQuantity
            let compDoseRate  = comp |> Component.getDoseRate :> QT.IQuantity
            let compConc      = comp |> Component.getConcentration :> QT.IQuantity

            let adjust adj eqs = 
                match adj with
                | Dose.Adjust.Weight a 
                | Dose.Adjust.Length a
                | Dose.Adjust.Surface a ->
                    let adjust = a :> QT.IQuantity
                    [[compAdminRate; adjust; compDoseRate]] 
                    |> toProdEqs 
                    |> List.append eqs
                | Dose.Adjust.NoAdjust -> eqs

            let eqs =
                [[compAdminRate; compConc; drugAdminRate]
                 [compDoseRate; compConc; drugDoseRate]] 
                |> toProdEqs
                |> List.append eqs
                |> adjust adj
            comp
            |> getSubs 
            |> List.fold (contSubstEqs adj drug comp) eqs 

//      drugAdminRate = adjust x drugDoseRate 
        let contDrugEqs adj eqs drug =
            let drugAdminRate = drug |> getAdminRate :> QT.IQuantity
            let drugDoseRate  = drug |> getDoseRate :> QT.IQuantity

            let adjust adj eqs = 
                match adj with
                | Dose.Adjust.Weight a 
                | Dose.Adjust.Length a
                | Dose.Adjust.Surface a ->
                    let adjust = a :> QT.IQuantity
                    [[drugAdminRate; adjust; drugDoseRate]] 
                    |> toProdEqs 
                    |> List.append eqs
                | Dose.Adjust.NoAdjust -> eqs

            let eqs = eqs |> adjust adj

            drug 
            |> getComps
            |> List.fold (contCompEqs adj drug) eqs

//      substDrugQty = substDrugConc x drugTotal   
//      substCompQty = substCompConc x compTotal   
//      substDrugQty = substCompConc x compQty     
//      doseTotal = freq x doseQty     
//      doseQty = substDrugQty               
        let discSubstEqs adj drug comp freq eqs (s: Substance.T) =
            let substDrugQty = s.Quantity :> QT.IQuantity
            let substDrugConc = s.Concentration :> QT.IQuantity
            let drugTotal = drug |> getTotal :> QT.IQuantity
            let substCompQty = s.Substance.Quantity :> QT.IQuantity
            let substCompConc = s.Substance.Concentration :> QT.IQuantity
            let compTotal = comp |> Component.getTotal :> QT.IQuantity
            let compQty = comp |> Component.getQuantity :> QT.IQuantity
            let doseTotal = s.Dose.DoseTotal :> QT.IQuantity
            let doseQty = s.Dose.DoseQuantity

            [[substDrugQty; substDrugConc; drugTotal]
             [substCompQty; substCompConc; compTotal]
             [substDrugQty; substCompConc; compQty]
             [doseTotal; freq; doseQty]]
            |> toProdEqs
            |> List.append eqs

//      compQty = compConc x drugTotal   
//      compDose = compConc x prescrQty   
//      compDoseTotal = compConc x prescrTotal 
//      compDoseRate = compConc x prescrRate  
//      compDose = compDoseRate x time        
//      compDoseTotal = compDose x freq        
        let discCompEqs adj drug freq eqs (comp: Component.T) =
            let qty = drug |> getAdminQuantity :> QT.IQuantity
            let freq = freq :> QT.IQuantity
            let tot = drug |> getAdminTotal :> QT.IQuantity

            comp
            |> getSubs 
            |> List.fold (discSubstEqs adj drug comp freq) eqs 


        let discDrugEqs adj freq eqs drug =
            drug
            |> getComps
            |> List.fold (discCompEqs adj drug freq) eqs

            
//      substDrugQty = substDrugConc x drugTotal  
//      substDrugQty = substCompConc x compQty    
//      substCompQty = substCompConc x compTotal  
//      doseTotal = freq x doseQty    
//      doseQty = time x doseRate   
//      doseRate = substDrugConc x prescrRate 
//      doseQty = substDrugConc x prescrQty  
//      doseTotal = substDrugConc x prescrTotal
        let timedSubstEqs adj drug comp freq time eqs (subst: Substance.T) =
            let substDrugQty = subst |> Substance.getDrugQuantity :> QT.IQuantity
            let substDrugConc = subst |> Substance.getDrugConcentration :> QT.IQuantity
            let drugTotal = drug |> getTotal :> QT.IQuantity
            let susbstCompConc = subst |> Substance.getComponentConcentration :> QT.IQuantity
            let compQty = comp |> Component.getQuantity :> QT.IQuantity
            let substCompQty = subst |> Substance.getComponentQuantity :> QT.IQuantity
            let compTotal = comp |> Component.getTotal :> QT.IQuantity  
            let doseTotal = subst |> Substance.getDoseTotal :> QT.IQuantity
            let doseQty = subst |> Substance.getDoseQuantity :> QT.IQuantity
            let doseRate = subst |> Substance.getDoseRate :> QT.IQuantity
            let adminRate = drug |> getAdminRate :> QT.IQuantity
            let adminQty = drug |> getAdminQuantity :> QT.IQuantity
            let adminTotal = drug |> getAdminTotal :> QT.IQuantity

            [[substDrugQty; substDrugConc; drugTotal]]
            |> toProdEqs
            |> List.append eqs

//      compQty = compConc x drugTotal  
//      compDose = compConc x prescrQty  
//      compDoseTotal = compConc x prescrTotal
//      compDoseRate = compConc x prescrRate 
//      compDose = compDoseRate x time       
//      compDoseTotal = compDose x freq       
        let timedCompEqs adj drug freq time eqs (comp: Component.T) =
            let compQty = comp |> Component.getQuantity :> QT.IQuantity
            let compConc = comp |> Component.getConcentration :> QT.IQuantity
            let drugTotal = drug |> getTotal :> QT.IQuantity
            let compDose = comp |> Component.getAdminQuantity :> QT.IQuantity
            let compConc = comp |> Component.getConcentration :> QT.IQuantity
            let adminQty = drug |> getAdminQuantity :> QT.IQuantity
            let compDoseTotal = comp |> Component.getDoseTotal :> QT.IQuantity
            let adminTotal = drug |> getAdminTotal :> QT.IQuantity
            let compDoseRate = comp |> Component.getDoseRate :> QT.IQuantity

            let eqs =
                [[compQty; compConc; drugTotal]] 
                |> toProdEqs
                |> List.append eqs
            comp
            |> getSubs 
            |> List.fold (timedSubstEqs adj drug comp freq time) eqs 

        let timedDrugEqs adj freq time eqs drug =
            drug
            |> getComps
            |> List.fold (timedCompEqs adj drug freq time) eqs
            
        let getSumEq (d: T) =
            d
            |> getComps
            |> List.map(fun c -> c.Quantity :> QT.IQuantity)
            |> QT.toSumEq


    module Lab =
        type T = { Name: string; Quantity: int }
        type Create = string -> int -> T option

        let create: Create = 
            fun n q -> { Name = n; Quantity = q } |> Some

    type T = 
        | DrugOrderable of Drug.T
        | LabOrderable of Lab.T
        | OrderableComposition of T * T list

    let create c =
        function
        | [] -> None
        | x::[] -> x |> c |> Some
        | x::xs -> OrderableComposition (x |> c, xs |> List.map c) |> Some

    let toEquations prescr ord adj =
        let eqs = 
            match ord with 
            | DrugOrderable d -> [(Drug.getSumEq d)]
            | LabOrderable _ -> []

        match prescr |> PN.getPrescription with
        | PN.Process -> []
        | PN.Continuous -> 
            match ord with
            | DrugOrderable d -> d |> Drug.contDrugEqs adj eqs
            | LabOrderable _ -> eqs
        | PN.Discontinuous freq ->
            match ord with
            | DrugOrderable d -> d |> Drug.discDrugEqs adj freq eqs
            | LabOrderable _ -> eqs
        | PN.Timed (freq, time) ->
            match ord with
            | DrugOrderable d -> d |> Drug.timedDrugEqs adj freq time eqs
            | LabOrderable _ -> eqs


module BirthDate =
    open System

    module WV = WrappedValue
    module WDT = WrappedDateTime

    [<Literal>]
    let MaxAge = 130
        
    type T =
        | BirthDate of WrappedDateTime.T
        | NoBirthDate

    let isValid dt = 
        let minDt = DateTime.Now.AddYears(-MaxAge)
        let maxDt = DateTime.Now
        dt > minDt && dt <= maxDt

    let create dt =
        match dt |> WrappedDateTime.create id isValid with
        | Some dt' -> dt' |> BirthDate
        | None -> NoBirthDate

    let value =
        function
        | BirthDate dt -> dt |> WV.value
        | NoBirthDate -> DateTime.MaxValue 

    let stringValue format dt = 
        match dt |> value with
        | dt' when dt' = DateTime.MaxValue -> String.Empty
        | dt' -> dt'.ToString()
        
    type T with
        static member Create = create 


module Person =
    open System

    open Informedica.Utilities

    module WV = WrappedValue
    module WS = WrappedString
    module WD = WrappedDateTime
    module BD = BirthDate

    let (<!>) = Result.liftR
    let (<*>) = Result.applyR

    module Messages = 
        type T =
            | CreatedId of string
            | CreatedFirstName of string
            | CouldNotCreateId of string
            | CouldNotCreateFirstName of string


    module PersonDto =

        type T () =
            member val Id: string = null with get, set
            member val FirstName : string = null with get, set
//            member val Initials : string list = [] with get, set
//            member val LastName : string = null with get, set
//            member val BirthDate : Nullable<DateTime> = Nullable() with get, set


    module Id =

        [<CustomEquality; NoComparison>]
        type T = 
            | Id of WS.String10.T
            | NoId

            override this.GetHashCode() = hash this

            override this.Equals(other) =
                match other with
                | :? T as id -> 
                    match this, id with
                    | Id ws1, Id ws2 -> ws1 |> WrappedValue.equals ws2
                    | _ -> false
                | _ -> false

        let create s = 
            let map = function
                | WS.Messages.StringIsNull -> Messages.CouldNotCreateId "because string was null"
                | WS.Messages.CreatedString _ -> Messages.CreatedId s
                | WS.Messages.StringLargerThan n -> Messages.CouldNotCreateId (s + " was larger than " + string n)

            match s |> WS.String10.create with
            | Result.Failure errs -> (NoId, errs) |> Result.Success
            | Result.Success (s10, msgs) -> (s10 |> Id, msgs) |> Result.Success
            |> Result.mapMessagesR map

        let apply f = function
            | Id s10 -> s10 |> WS.String10.apply f
            | NoId -> "" |> f

        let get = apply id
        let change f x = x |> apply f |> create


    module FirstName =

        type T = FirstName of  WS.CapitalizedName.T

        let create s =
            let map = function 
                | WS.Messages.StringIsNull -> Messages.CouldNotCreateFirstName "because string was null"
                | WS.Messages.CreatedString s -> Messages.CreatedFirstName s
                | WS.Messages.StringLargerThan _ -> failwith "unexpected message"

            match s |> WS.CapitalizedName.create with
            | Result.Failure errs -> errs |> Result.Failure
            | Result.Success (n, msgs) -> (n |> FirstName, msgs) |> Result.Success
            |> Result.mapMessagesR map

        let apply f (FirstName n) = n |> WS.CapitalizedName.apply f

        let get = apply id
        let change f x = x |> apply f |> create


//    type SingleCapital = WS.SingleCapital
//    type FirstName = WS.CapitalizedName
//    type LastName = WS.CapitalizedName
//    type BirthDate = BirthDate.T

    [<StructuralEquality; NoComparison>]
    type PersonName =
        { 
            FirstName: FirstName.T
//            Initials: SingleCapital list
//            LastName: LastName
        }

    
    [<CustomEquality; NoComparison>]
    type T = 
        { 
            Id: Id.T
            Name: PersonName
//            BirthDate: BirthDate
        }

        override this.GetHashCode() = hash this.Id

        override this.Equals(other) =
            match other with
            | :? T as p -> p.Id = this.Id
            | _ -> false


    let create (dto: PersonDto.T) =

        let createPerson id name = { Id = id; Name = name } 
        let createPersonName s =
            match s |> FirstName.create with
            | Result.Success (n, msgs) -> ({ FirstName = n }, msgs) |> Result.Success
            | Result.Failure errs -> Result.Failure errs

        createPerson
        <!> Id.create dto.Id
        <*> createPersonName dto.FirstName


module Patient =

    open Informedica.Utilities

    module WV = WrappedValue
    module WS = WrappedString
    module MP = Multipliers
    module UN = Unit
    module VR = Variable

    [<NoComparison>]
    type HospitalId = 
        | HospitalId of WS.String10
        | NoHospitalId

    type Person = 
        | Person of Person.T
        | Anonymous

    type Weight = Weight of VR.T
    type Length = Length of VR.T
    type BSA = BSA of VR.T

    [<CustomEquality; NoComparison>]
    type T = 
        {
            HospitalId: HospitalId
            Person: Person
            Weight: Weight option
            Length: Length option
            BodySurfaceArea: BSA option
        } with

        override this.GetHashCode() = this.HospitalId |> hash

        override this.Equals(other) = 
            match other with
            | :? T as pat -> pat.HospitalId = this.HospitalId
            | _ -> false

    let (>>=) o f = Option.bind f o

    let createHospitalId s = 
        match s|> WS.createString10 with
        | Some n -> n |> HospitalId |> Some 
        | None -> None

    let createWeight q u =
        let weight = [] |> VR.withUnit UN.gram |> VR.create
        weight |> VR.setMin 200N
        weight |> VR.setMax (700N * MP.kilo)

        match u |> UN.weightFromString with
        | Some wu -> weight |> VR.setValue [q]
        | None -> ()
        
        weight |> Weight

    let getWeight (Weight w) = 
        match w |> VR.getUnitValues with
        | v::[] -> v | _ -> 0N

    let createLength q u =
        let length = [] |> VR.withUnit UN.centimeter |> VR.create
        length |> VR.setMin 30N
        length |> VR.setMax 300N

        match u |> UN.distanceFromString with
        | Some du -> length |> VR.setValue [q]
        | None -> ()
        
        length |> Length

    let getLength (Length l) =
        match l |> VR.getUnitValues with
        | v::[] -> v | _ -> 0N

    let newPatient =
        {
            HospitalId = NoHospitalId
            Person = Anonymous
            Weight = None
            Length = None
            BodySurfaceArea = None
        } |> Some

    let setHospitalId =
        fun id p ->
            match id |> createHospitalId with
            | Some id' -> { p with HospitalId = id' } |> Some
            | None -> None

    let setPerson =
        fun id name p ->
            match Person.create_opt id name with
            | Some person -> { p with Person =  person |> Person } |> Some
            | None -> None

    let setBirthDate =
        fun bd p ->
            let set bd =
                match p.Person with
                | Person person -> person.SetBirthDate bd |> Some
                | Anonymous -> None

            match bd |> set with
            | Some person -> { p with Person = person |> Person } |> Some
            | None -> None

    let setWeight =
        fun qt u p -> { p with Weight = u |> createWeight qt |> Some } |> Some

    let setLength =
        fun v u p -> { p with Length = u |> createLength v |> Some } |> Some

    let create id n bd =
        newPatient 
        >>= (id |> setHospitalId)
        >>= (setPerson id n)
        >>= (bd |> setBirthDate)


module Order =

    exception InvalidOrderException

    module OR = Orderable
    module PR = Prescription

    type T = 
        {
            Id: string // OrderId.T
            Patient: Patient.T
            Indications: string * string list //Indication.T * Indication.T list
            Orderable: Orderable.T
            Prescription: Prescription.T
            Route: string // Route.T
            StartDate: System.DateTime // StartDate.T
            StopDate: System.DateTime option // StopDate.T option
        }

    let applyToT f (x: T) = x |> f

    let getPatient = applyToT (fun x -> x.Patient)
    let getOrderable = applyToT (fun x -> x.Orderable)
    let getPrescription = applyToT (fun x -> x.Prescription)
    
    let orderToEquations o = 
        let pat, ord, pr = o |> getPatient, o |> getOrderable, o |> getPrescription 

        []


module EQ = Equation
module VR = Variable
module VC = ValuesUnitCombi
module UN = Unit
module SV = Solver
module QT = Quantities
module PR = Prescription
module OR = Orderable

let tot = [12N] |> VR.withUnit UN.Units.milliGram |> VR.per 1N UN.Units.day |> VR.create
let freq = VR.emptyVar()
let qty = [1N;2N;3N;4N;6N;12N] |> VR.withUnit UN.Units.milliGram |> VR.create
let rate = [] |> VR.withUnit UN.Units.milliLiter |> VR.per 1N UN.Units.hour |> VR.create
let time = [] |> VR.withUnit UN.Units.hour |> VR.create
freq |> VR.setIncr 1N
rate |> VR.setIncr (1N/10N)
rate |> VR.setMax 20N

let e1 = [tot;freq;qty] |> EQ.createProductEq
let e2 = [qty;rate;time] |> EQ.createProductEq


let (>>=) m k = Railway.bind k m
let lift2 k m1 m2 = 
    match m1 with 
    | Railway.Success m1' -> k m1' m2 |> Railway.succeed  
    | Railway.Failure f -> 
        f @ [""] |> ignore; // Necessary to avoid value restriction exception
        f |> Railway.fail

let setSubstDrugQty = lift2 Product.Substance.setQuantity
let setSubstDoseTot = lift2 OR.Drug.Substance.setDoseTotal
let setSubstDoseTotIncr = lift2 OR.Drug.Substance.setDoseTotalIncr


let pcmS = Product.Substance.create "paracetamol"

pcmS |> Railway.succeed
>>= setSubstDrugQty (QT.Quantity.create [240N] "milligram")

let createSubst n qty qun cqty cun = 
    let setQty = lift2 Product.Substance.setQuantity
    let setConc = lift2 Product.Substance.setConcentration

    Product.Substance.create n 
    |> Railway.succeed
    >>= setQty (QT.Quantity.create [qty] qun)
    >>= setConc (QT.Concentration.create [cqty] qun cun)

createSubst "dopamine" 200N "milligram" 5N "milliliter"


let freq2 = QT.Frequency.freq [] |> QT.Frequency.per 1N "day" |> Railway.getSuccess
freq2 :> QT.IQuantity |> QT.setIncr 2N :?> QT.Frequency.T

freq2 |> QT.Frequency.setIncr 3N


let doseTot = QT.Total.create [] "milligram" 1N "day" |> Railway.getSuccess
doseTot |> QT.setIncr 1N
doseTot |> QT.setMax 10N

let dt = QT.Total.total [] "milligram" |> QT.Total.per 1N "day"
let dt2 = QT.TotalAdjust.total [] "milligram" |> QT.TotalAdjust.adjustBy 1N "kilogram" |> QT.TotalAdjust.per 1N "day"

failwithf "test"

type DomainMessages = 
    | ``Name cannot be null``
    | ``Value must be positive``

let msg = ``Name cannot be null``

let map = function
    | ``Name cannot be null`` -> printfn "Name cannot be positibe"
    | ``Value must be positive`` -> ()
