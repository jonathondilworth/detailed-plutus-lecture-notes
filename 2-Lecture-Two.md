# Lecture Two

> "Haskell is useless"
> — Simon Peyton Jones

So, let's learn how to write smart contracts in Haskell! [Watch this video](https://youtu.be/iSmkqocn0oQ), it makes sense.

### 1. Introduction

Within this set of lecture notes, some information about UTxO (or extended UTxOs if you prefer) is initially discussed (the constraints required for consumption). The notion of on-chain and off-chain scripts is discussed. A reminder of what a EUTxO model is, is presented in detail (including information about datum, redeemers and context). We discuss some of the exercises demonstrated within the second lecture of the second cohort of the Plutus Pioneer Program and ...

#### 1.1 Catch Up | EUTxO - Additional Information

Previously a pioneer brought up the notion of "what if we didn't add any kind of 'end-point' to our smart contract? Would the funds just get stuck within the contract?". Unfortunately, that would appear to be the case (if the script was coded in such a manner that there was no stopping criteria implemented". This is to say that: in order to change the state of the blockchain (to consume any given UTxO) a transaction must be executed on-chain (and validated), such that the previous UTxO is consumed and the next UTxO is created. UTxOs will never spring into action themselves.

* New transactions are generated (initiated) by a wallet (which is essentially a collection of keys, as your wallet has the private key to some UTxOs outputs 'public' key (which are thought of as scripts and redeemers).
* The state of any given UTxO can only be changed if the outputs are verified by satisfying the arbitrary logic held at the script address, using the required redeemer (input parameters).
* You NEED to have some kind of 'close clause' - that could even just be 'expire after X blocks'.
* However, off-chain (wallet) logic can do some sophisticated stuff (but we'll get to that later).
* On-chain Logic is about Validation (can a UTxO be consumed?)
* Off-chain logic is about initating transactions that effect the state of any given UTxO for which you hold the required redeemer (think of it somewhat like a key to sign a digital signature, except, it's not..)


### 2. The Difference: (E)UTxO VS UTxO

A simple UTxO model usually takes a hash of some form of public key and uses this as the address. The redeemer for the UTxO model can then simply derive the public key and then sign the transaction using the paired private key (ensuring they are in fact the person who holds the private key in their wallet; and so a UTxO is consumed by signing the UTxO and a new UTxO is created with inputs and outputs.

-

(Extended)UTxOs have a number of address types, one of which is a script addresses. At this address a smart contract can exist on-chain that can run arbitrary logic.

Transactions that want to consume an (E)UTxO sitting at a script address are validated by a node, the node will run the script and depending on the result of the script (typically TRUE / FALSE, but other more complicated outputs can exist I believe) consumption is permitted or non-permitted.

### 2.1 Redeemers

A redeemer is an arbitrary piece of data that is fed to the script (similar to a set of parameters), a the script requires this data to satisfy the constraints and reach a deterministic outcome (if the script has been written properly).

### 2.2 Datum

Datum is a 'string', a piece of data that sits at the output of any given (E)UTxO. It's great for providing an area to store the output state of a UTxO script or possibly even a 'linked-list' of (E)UTxOs.

### 2.3 Context

This is essentially the scope of the script. Do we allow the script to see almost nothing, or do we allow it to see the entire blockchain? In the case of cardano, it can see the the scope of the current UTxO, it's inputs and it's outputs.

### 2.4 Plutus Script

Three pieces of data required to create a valid Plutus script:

1. Redeemer
2. Datum
3. Context

-

### 3. Data Types In Plutus (as implemented in Haskell)

Haskell data type: Data (at least at the low level implementation of Plutus (plutus-core), in real life nobody uses Data as the data type in a script, as there are better alternatives. But it is better to learn from first principals.

##### Haskell Data Type: Data

	{-# LANGUAGE BangPatterns       #-}
	{-# LANGUAGE DeriveAnyClass     #-}
	{-# LANGUAGE DerivingStrategies #-}
	{-# LANGUAGE LambdaCase         #-}
	{-# LANGUAGE MultiWayIf         #-}
	{-# LANGUAGE OverloadedStrings  #-}
	{-# LANGUAGE ViewPatterns       #-}
	
	-- | Notes - JD
	-- This is a low level 'data type' within PlutusCore (somewhat confusing since plutus-core
	-- is actually System F Omega + Recerive Data Types, whilst PlutusCore is the high level
	-- implementation in Haskell which uses plutus-tx to compile to System F. So, PlutusCore is
	-- actually just Plutus?
	
	module PlutusCore.Data (Data (..)) where
	
	import           Codec.CBOR.Decoding       (Decoder)
	import qualified Codec.CBOR.Decoding       as CBOR
	import qualified Codec.CBOR.Term           as CBOR
	import           Codec.Serialise           (Serialise (decode, encode))
	import           Codec.Serialise.Decoding  (decodeSequenceLenIndef, decodeSequenceLenN)
	import           Control.DeepSeq           (NFData)
	import           Control.Monad.Except
	import           Data.Bifunctor            (bimap)
	import qualified Data.ByteString           as BS
	import           Data.Text.Prettyprint.Doc
	import           GHC.Generics
	import           Prelude
	
	-- | A generic "data" type.
	--
	-- The main constructor 'Constr' represents a datatype value in sum-of-products
	-- form: @Constr i args@ represents a use of the @i@th constructor along with its arguments.
	--
	-- The other constructors are various primitives.
	
	-- | J.D Notes: Map [(Data, Data)] are key-value pairs which represent tuples of (data, data)
	-- If I understand correctly, a list of key-value pairs = map
	-- Each of these | are constructors.
	
	
	data Data =
	      Constr Integer [Data]
	    | Map [(Data, Data)]
	    | List [Data]
	    | I Integer
	    | B BS.ByteString
	    deriving stock (Show, Eq, Ord, Generic)
	    deriving anyclass (NFData)
	
	instance Pretty Data where
	    pretty = \case
	        Constr _ ds -> angles (sep (punctuate comma (fmap pretty ds)))
	        Map entries -> braces (sep (punctuate comma (fmap (\(k, v) -> pretty k <> ":" <+> pretty v) entries)))
	        List ds     -> brackets (sep (punctuate comma (fmap pretty ds)))
	        I i         -> pretty i
	        B b         -> viaShow b
	
	{- Note [Encoding via Term]
	We want to write a custom encoder/decoder for Data (i.e. not use the Generic version), but actually
	doing this is a pain. So instead we go via the CBOR 'Term' representation, which lets us process a
	more structured representation, which is a lot easier.
	-}
	
	instance Serialise Data where
	    -- See Note [Encoding via Term]
	    encode = CBOR.encodeTerm . toTerm
	    decode = decodeData
	
	{- Note [CBOR alternative tags]
	We've proposed to add additional tags to the CBOR standard to cover (essentially) sum types.
	This is exactly what we need to encode the 'Constr' constructor of 'Data' in an unambiguous way.
	The tags aren't *quite* accepted yet, but they're clearly going to accept so we might as well
	start using them.
	The scheme is:
	- Alternatives 0-6 -> tags 121-127
	- Alternatives 7-127 -> tags 1280-1400
	- Any alternatives, including those that don't fit in the above -> tag 102 followed by an integer for the actual alternative.
	-}
	
	-- | Turn Data into a CBOR Term.
	toTerm :: Data -> CBOR.Term
	toTerm = \case
	    -- See Note [CBOR alternative tags]
	    Constr i ds | 0 <= i && i < 7   -> CBOR.TTagged (fromIntegral (121 + i)) (CBOR.TList $ fmap toTerm ds)
	    Constr i ds | 7 <= i && i < 128 -> CBOR.TTagged (fromIntegral (1280 + (i - 7))) (CBOR.TList $ fmap toTerm ds)
	    Constr i ds | otherwise         -> CBOR.TTagged 102 (CBOR.TList $ CBOR.TInteger i : fmap toTerm ds)
	    Map es                          -> CBOR.TMap (fmap (bimap toTerm toTerm) es)
	    List ds                         -> CBOR.TList $ fmap toTerm ds
	    I i                             -> CBOR.TInteger i
	    B b                             -> CBOR.TBytes b
	
	{- Note [Definite and indefinite forms of CBOR]
	CBOR is annoying and you can have both definite (with a fixed length) and indefinite lists, maps, etc.
	So we have to be careful to handle both cases when decoding. When encoding we simply don't make
	the indefinite kinds.
	-}
	
	-- | Turn a CBOR Term into Data if possible.
	decodeData :: forall s. Decoder s Data
	decodeData = CBOR.peekTokenType >>= \case
	  CBOR.TypeUInt         -> I <$> CBOR.decodeInteger
	  CBOR.TypeUInt64       -> I <$> CBOR.decodeInteger
	  CBOR.TypeNInt         -> I <$> CBOR.decodeInteger
	  CBOR.TypeNInt64       -> I <$> CBOR.decodeInteger
	  CBOR.TypeInteger      -> decodeBoundedInteger
	
	  CBOR.TypeBytes        -> decodeBoundedBytes
	  CBOR.TypeBytesIndef   -> decodeBoundedBytes
	
	  CBOR.TypeListLen      -> decodeList
	  CBOR.TypeListLen64    -> decodeList
	  CBOR.TypeListLenIndef -> decodeList
	
	  CBOR.TypeMapLen       -> decodeMap
	  CBOR.TypeMapLen64     -> decodeMap
	  CBOR.TypeMapLenIndef  -> decodeMap
	
	  CBOR.TypeTag          -> decodeConstr
	  CBOR.TypeTag64        -> decodeConstr
	
	  t                     -> fail ("Unrecognized value of type " ++ show t)
	
	decodeBoundedInteger :: Decoder s Data
	decodeBoundedInteger = do
	  i <- CBOR.decodeInteger
	  unless (inBounds i) $ fail "Integer exceeds 64 bytes"
	  pure $ I i
	  where
	  bound :: Integer
	  -- The maximum value of a 64 byte unsigned integer
	  bound = 2 ^ (64 * 8 :: Integer) - 1
	  inBounds x = (x <= bound) && (x >= -1 - bound)
	
	decodeBoundedBytes :: Decoder s Data
	decodeBoundedBytes =  do
	  b <- CBOR.decodeBytes
	  if BS.length b <= 64
	    then pure $ B b
	    else fail $ "ByteString exceeds 64 bytes"
	
	decodeList :: Decoder s Data
	decodeList = List <$> decodeListOf decodeData
	
	decodeListOf :: Decoder s x -> Decoder s [x]
	decodeListOf decoder = CBOR.decodeListLenOrIndef >>= \case
	  Nothing -> decodeSequenceLenIndef (flip (:)) [] reverse   decoder
	  Just n  -> decodeSequenceLenN     (flip (:)) [] reverse n decoder
	
	decodeMap :: Decoder s Data
	decodeMap = CBOR.decodeMapLenOrIndef >>= \case
	  Nothing -> Map <$> decodeSequenceLenIndef (flip (:)) [] reverse   decodePair
	  Just n  -> Map <$> decodeSequenceLenN     (flip (:)) [] reverse n decodePair
	  where
	  decodePair = (,) <$> decodeData <*> decodeData
	
	-- See note [CBOR alternative tags] for the encoding scheme.
	decodeConstr :: Decoder s Data
	decodeConstr = CBOR.decodeTag64 >>= \case
	  102 -> decodeConstrExtended
	  t | 121 <= t && t < 128 ->
	         Constr (fromIntegral t - 121) <$> decodeListOf decodeData
	  t | 1280 <= t && t < 1401 ->
	         Constr ((fromIntegral t - 1280) + 7) <$> decodeListOf decodeData
	  t -> fail ("Unrecognized tag " ++ show t)
	  where
	  decodeConstrExtended = do
	    lenOrIndef <- CBOR.decodeListLenOrIndef
	    i <- CBOR.decodeWord64
	    xs <- case lenOrIndef of
	      Nothing -> decodeSequenceLenIndef (flip (:)) [] reverse       decodeData
	      Just n  -> decodeSequenceLenN     (flip (:)) [] reverse (n-1) decodeData
	    pure $ Constr (fromIntegral i) xs

-

If we would like to see information about the various Data constructors, we can do so by:

1. opening up a terminal window.
2. navigating to plutus-pioneer-program
3. opening the cabal.project file
4. grabbing the current git checkout key for Plutus,
5. then navigating to the Plutus repo
6. git checkout <key>
7. starting a nix-shell,
8. navigating back to week02
9. starting cabal repl & entering the following code:

-

	import PlutusTx
	:i Data
	
	Return Value:
	
	Prelude PlutusTx Week02.Burn> :i Data
	type Data :: *
	data Data
	  = Constr Integer [Data]
	  | Map [(Data, Data)]
	  | List [Data]
	  | I Integer
	  | B bytestring-0.10.12.0:Data.ByteString.Internal.ByteString
	  	-- Defined in ‘plutus-core-0.1.0.0:PlutusCore.Data’
	instance Eq Data
	  -- Defined in ‘plutus-core-0.1.0.0:PlutusCore.Data’
	instance Ord Data
	  -- Defined in ‘plutus-core-0.1.0.0:PlutusCore.Data’
	instance Show Data
	  -- Defined in ‘plutus-core-0.1.0.0:PlutusCore.Data’
	instance IsData Data -- Defined in ‘PlutusTx.IsData.Class’

Setting a Data value (simple Integer):

	import PlutusTx
	I 42
	-- | What type is our new piece of data I, which is = to 42?
	:t I 42
	-- | Return Value:
	I 42 :: Data
	-- | As we can see it is of type Data
	
Setting a data value (of type Bytestring):

	-- | Normal strings in Haskell are just sequences of characters
	-- to use bytestrings, we need to import a module call XOverloadedStrings
	import PlutusTx
	set -XOverloadedStrings
	B "Hell"
	:t B "Hello"
	>> B "Hello" :: Data
	
*You Get The Idea...*

*Very similar to JSON apparently...*





