> {-# LANGUAGE OverloadedStrings #-}

| In this tutorial you will learn how to use this library. The /Simple example/
section should be enough to get you going, but you can keep reading if you want
to better understand how to deal with complex parsing scenarios.

You may import this module and try the subsequent examples as you go.


> module Control.Proxy.Attoparsec.Tutorial
>   (-- * Simple example
>    -- $example-simple
>
>    -- * Parsing control proxies
>    -- ** Handling parser errors
>    -- $example-errors
>
>    -- ** Composing
>    -- $example-compose-control
>
>    -- ** Custom behavior
>    -- $example-control-custom
>
>    -- * Try for yourself
>    -- $example-try
>     Name(..)
>   , hello
>   , input1
>   , input2
>   , helloPipe1
>   , helloPipe2
>   , helloPipe3
>   , helloPipe4
>   , helloPipe5
>   , helloPipe6
>   , skipPartialResults
>   ) where
>
> import Control.Proxy
> import Control.Proxy.Attoparsec
> import Control.Proxy.Trans.Either
> import Data.Attoparsec.Text
> import Data.Text
>
> data Name = Name Text
>           deriving (Show, Eq)
>
> hello :: Parser Name
> hello = fmap Name $ "Hello " .*> takeWhile1 (/='.') <*. "."
>
> input1 :: [Text]
> input1 =
>   [ "Hello Kate."
>   , "Hello Mary.Hello Jef"
>   , "f."
>   , "Hel"
>   , "lo Tom."
>   ]
>
> input2 :: [Text]
> input2 =
>   [ "Hello Amy."
>   , "Hello, Hello Tim."
>   , "Hello Bob."
>   , "Hello James"
>   , "Hello"
>   , "Hello World."
>   , "HexHello Jon."
>   , "H"
>   , "ello Ann"
>   , "."
>   , "Hello Jean-Luc."
>   ]
>
> helloPipe1 :: (Proxy p, Monad m) => () -> Pipe p Text Name m r
> helloPipe1 = parserInputD >-> parserD hello
>
> helloPipe2 :: (Proxy p, Monad m) => () -> Pipe p Text Name m r
> helloPipe2 = parserInputD >-> retryLeftovers >-> parserD hello
>
> helloPipe3 :: (Proxy p, Monad m) => () -> Pipe (EitherP BadInput p) Text Name m r
> helloPipe3 = parserInputD >-> throwParsingErrors >-> parserD hello
>
> helloPipe4 :: (Proxy p, Monad m) => () -> Pipe (EitherP BadInput p) Text Name m r
> helloPipe4 = parserInputD >-> limitInputLength 10 >-> parserD hello
>
> helloPipe5 :: (Proxy p, Monad m) => () -> Pipe (EitherP BadInput p) Text Name m r
> helloPipe5 = parserInputD >-> limitInputLength 10 >-> retryLeftovers >-> parserD hello
>
> helloPipe6 :: (Proxy p, Monad m) => () -> Pipe p Text Name m r
> helloPipe6 = parserInputD >-> skipPartialResults >-> parserD hello
>
> skipPartialResults
>  :: (Monad m, Proxy p, AttoparsecInput a)
>  => ParserStatus a
>  -> p (ParserStatus a) (ParserSupply a) (ParserStatus a) (ParserSupply a) m r
> skipPartialResults = runIdentityK . foreverK $ go
>   where go x@(Parsing _) = request x >>= \(_, a) -> respond (Start, a)
>         go x             = request x >>= respond


$example-simple

We'll write a simple 'Parser' that turns 'Text' like /“Hello John Doe.”/
into @'Name' \"John Doe\"@, and then make a 'Pipe' that turns
those 'Text' values flowing downstream into 'Name' values flowing
downstream using that 'Parser'.

In this example we are using 'Text', but we may as well use
'Data.ByteString.ByteString'. Also, the 'OverloadedStrings' language
extension lets us write our parser easily.

  > {-# LANGUAGE OverloadedStrings #-}
  >
  > import Control.Proxy
  > import Control.Proxy.Attoparsec
  > import Control.Proxy.Trans.Either
  > import Data.Attoparsec.Text
  > import Data.Text
  >
  > data Name = Name Text
  >           deriving (Show)
  >
  > hello :: Parser Name
  > hello = fmap Name $ "Hello " .*> takeWhile1 (/='.') <*. "."

We are done with our parser, now lets make a simple parsing 'Pipe' with it.

  > helloPipe1 :: (Proxy p, Monad m) => () -> Pipe p Text Name m r
  > helloPipe1 = parserInputD >-> parserD hello

As the type indicates, this 'Pipe' receives 'Text' values from upstream and
sends 'Name' values downstream. This 'Pipe' is made of two smaller
two smaller cooperating 'Proxy's:

  1. 'parserInputD': Prepares @a@ input received from upstream to be
     consumed by a downstream parsing 'Proxy'.

  2. 'parserD': Repeatedly runs a given @'Parser' a b@ on input 'a' from
     upstream, and sends 'b' values downstream.

We need some sample input to test our simple 'helloPipe1'.

  > input1 :: [Text]
  > input1 =
  >   [ "Hello Kate."
  >   , "Hello Mary.Hello Jef"
  >   , "f."
  >   , "Hel"
  >   , "lo Tom."
  >   ]

We'll use @'fromListS' input1@ as our input source, which sends
downstream one element from the list at a time. We'll call each of these
elements a /chunk/. So, @'fromListS' input1@ sends 5 /chunks/ of
'Text' downstream.

Notice how some of our /chunks/ are not, by themselves, complete inputs
for our 'hello' 'Parser'. This is fine; we want to be able to feed the
'Parser' with either partial or complete input as soon as it's
received from upstream. More input will be requested when needed.
Attoparsec's 'Parser' handles partial parsing just fine.

  >>> runProxy $  fromListS input1 >-> helloPipe1 >-> printD
  Name "Kate"
  Name "Mary"
  Name "Jeff"
  Name "Tom"

We have accomplished our simple goal: We've made a 'Pipe' that parses
downstream flowing input using our 'Parser' 'hello'.


$example-errors

Let's try with some more complex input.

  > input2 :: [Text]
  > input2 =
  >   [ "Hello Amy."
  >   , "Hello, Hello Tim."
  >   , "Hello Bob."
  >   , "Hello James"
  >   , "Hello"
  >   , "Hello World."
  >   , "HexHello Jon."
  >   , "H"
  >   , "ello Ann"
  >   , "."
  >   , "Hello Jean-Luc."
  >   ]

  >>> runProxy $ fromListS input2 >-> helloPipe1 >-> printD
  Name "Amy"
  Name "Bob"
  Name "JamesHelloHello World"
  Name "Ann"
  Name "Jean-Luc"

The simple @helloPipe1@ we built skips /chunks/ of input that fail to be
parsed, and then continues parsing new input. That approach might be
enough if you are certain your input is always well-formed, but
sometimes you may prefer to act differently on these extraordinary
situations.

Instead of just using 'parserInputD' and 'parserD' to build our
'helloPipe1', we could have used an additional 'Proxy' in between them to
handle these situations. The module "Control.Proxy.Attoparsec.Control"
exports some useful 'Proxy's that serve this purpose. The default
behavior just mentioned resembles the one provided by
'skipMalformedChunks'.

Here are some other examples:

['retryLeftovers']
   On parsing failures, keep retrying with any left-over input, skipping
   individual bits each time. If there are no left-overs, then more
   input is requests form upstream.

  > helloPipe2 :: (Proxy p, Monad m) => () -> Pipe p Text Name m r
  > helloPipe2 = parserInputD >-> retryLeftovers >-> parserD hello

  >>> runProxy $ fromListS input2 >-> helloPipe2 >-> printD
  Name "Amy"
  Name "Tim"
  Name "Bob"
  Name "JamesHelloHello World"
  Name "Jon"
  Name "Ann"
  Name "Jean-Luc"

['throwParsingErrors']
  When a parsing error arises, aborts execution by throwing
  'MalformedInput' in the 'EitherP' proxy transformer.

  > helloPipe3 :: (Proxy p, Monad m) => () -> Pipe (EitherP BadInput p) Text Name m r
  > helloPipe3 = parserInputD >-> throwParsingErrors >-> parserD hello

  >>> runProxy . runEitherK $ fromListS input2 >-> helloPipe3 >-> printD
  Name "Amy"
  Left (MalformedInput {miParserErrror = ParserError {errorContexts = [], errorMessage = "Failed reading: takeWith"}})

[@'limitInputLength' n@]
  If a @'Parser' a b@ has consumed input @a@ of length longer than
  @n@ without producing a @b@ value, and it's still requesting more
  input, then throw 'InputTooLong' in the 'EitherP' proxy transformer.

  > helloPipe4 :: (Proxy p, Monad m) => () -> Pipe (EitherP BadInput p) Text Name m r
  > helloPipe4 = parserInputD >-> limitInputLength 10 >-> parserD hello

  >>> runProxy . runEitherK $ fromListS input2 >-> helloPipe4 >-> printD
  Name "Amy"
  Name "Bob"
  Left (InputTooLong {itlLenght = 11})

  Notice that by default, as mentioned earlier, parsing errors are
  ignored by skipping the malformed /chunk/. That's why we didn't get any
  complaint about the malformed input between /“Amy”/ and /“Bob”/.


$example-compose-control

These 'Proxy's that control the parsing behavior can be easily plugged
together with @('>->')@ to achieve a combined functionality. Keep in
mind that the order in which these 'Proxy's are used is important.

Suppose you don't want to parse inputs of length longer than 10, and on
parsing failures, you want to retry feeding the parser with any
left-overs.

  > helloPipe5 :: (Proxy p, Monad m) => () -> Pipe (EitherP BadInput p) Text Name m r
  > helloPipe5 = parserInputD >-> limitInputLength 10 >-> retryLeftovers >-> parserD hello

  >>> runProxy . runEitherK $ fromListS input2 >-> helloPipe5 >-> printD
  Name "Amy"
  Name "Tim"
  Name "Bob"
  Left (InputTooLong {itlLenght = 11})


$example-control-custom

In case the 'Proxy's provided by "Control.Proxy.Attoparsec.Control" are not
enough for your needs, you can create your custom parsing control 'Proxy'.

Through a parsing control 'Proxy',  @'ParserStatus' a@ values flow
upstream and @'ParserSupply' a@ values flow downstream.  A parsing control
'Proxy' simply replace these values or their flow to achieve its purpose. A
@'ParserStatus' a@ value received from downstream reports the status of a
'parserD' parsing 'Proxy', and in exchange, downstream expects a @'ParserSupply'
a@ value.

@'ParserSupply' a@ values carry raw input to be parsed and directives on how it
should be used. @'ParserSupply' a@ is just a type synonym for
@('SupplyUse', a)@. The @a@ value is the input chunk, and the 'SupplyUse' value
could be either 'Resume' if the parser currently waiting for input should be
fed, or 'Start', if a new parser should be started and fed the input,
effectively aborting any parsing activity currently waiting for more input.

See the documentation about 'ParserStatus' and 'ParserSupply' for more details.

Suppose you want to write a parsing control 'Proxy' that never provides
additional input to partial parsing results. Let's first take a look at the
type of this 'Proxy':

  > skipPartialResults
  >  :: (Monad m, Proxy p, AttoparsecInput a)
  >  => ParserStatus a
  >  -> p (ParserStatus a) (ParserSupply a) (ParserStatus a) (ParserSupply a) m r

Just like we said, @'ParserStatus' a@ values flow upstream and @'ParserSupply'
a@ values flow downstream.

Now to a simple implementation: If we receive @'Parsing' n@ from downstream,
then we know there is a partial parsing result waiting for more input. If we
were to respond to this request with @('Resume', a)@, then the partial parser
would continue consuming input, but if we change our response to @('Start', a)@,
then the partial parser would be aborted and a new parser would start
consuming the given input. A simple implementation is quite straightforward:

  > skipPartialResults = runIdentityK . foreverK $ go
  >   where go x@(Parsing _) = request x >>= \(_, a) -> respond (Start, a)
  >         go x             = request x >>= respond

We forward upstream the requests we get from downstream. However, in the case
of a @'Parsing' n@ status, we replace with 'Start' the first value in the
@'ParserSupply' a@ pair we receive from upstream before responding.

Now we can use this parsing control 'Proxy' with some simple input and see it
working.

  > helloPipe6 :: (Proxy p, Monad m) => () -> Pipe p Text Name m r
  > helloPipe6 = parserInputD >-> skipPartialResults >-> parserD hello

  >>> runProxy $ fromListS input1 >-> helloPipe6 >-> printD
  Name "Kate"
  Name "Mary"


$example-try

This module exports the following previous examples so that you can try
them.

