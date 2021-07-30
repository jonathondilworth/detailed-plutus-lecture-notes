# 3. Lecture Three: Introduction

in order to unloc a script address - Tx to run - 3 paras: datum, redeemer, context

Data can be used

but we need to use types:

* Data and Redeemer can be custom types, so long as they implement the IsData type
* Context uses ScriptContext

So far we've only looked at the data and redeemer and ignored the context:

Example:

	mkValidator :: () -> MyRedeemer -> ScriptContext -> Bool
	-- we can add a condition when we're creating the validator, in this case: x must == y
	mkValidator () (MyRedeemer x y) _ = traceIfFalse "Wrong Redeemer" $ x == y

However, we have ignored the context...



Data Type = Typed Version (Custom Types As long as it 

third argument = ScriptContext


Datum

Redeemer

Context: plutus-ldger-api

