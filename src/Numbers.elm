module Numbers exposing (..)

--import Basic

type alias Complex = { re : Float, im : Float }

nan : Float
nan = 0.0/0.0

polar r theta = { re = r * Basics.cos theta, im = r * Basics.sin theta }

add x y = { re = x.re + y.re, im = x.im + y.im }

sub x y = { re = x.re - y.re, im = x.im - y.im }

mul x y = { re = x.re * y.re - x.im * y.im, im = x.re * y.im + x.im * y.im }

div x y =
  let
    a = x.re * y.re + x.im * y.im
    b = x.im * y.re - x.re * y.im
    z = y.re * y.re + y.im * y.im
  in
    { re = a / z, im = b / z }

neg z = { re = negate z.re, im = negate z.im }

conj z = { re = z.re, im = negate z.im }

abs : Complex -> Float
abs z = rhypot z.re z.im

arg : Complex -> Float
arg z = Basics.atan2 z.im z.re

real z = z.re
imag z = z.im

inv : Complex -> Complex
inv z = div { re = 1.0, im = 0.0 } z

exp : Complex -> Complex
exp z = let sc = Basics.e ^ z.re in { re = sc * Basics.cos z.im, im = sc * Basics.sin z.im }

exp10 : Complex -> Complex
exp10 z = let x = Basics.logBase Basics.e 10.0 in exp { re = x * z.re, im = x * z.im }

log : Complex -> Complex
log z =
  let
    a = Basics.logBase Basics.e (abs z)
    p = Basics.atan2 z.im z.re
  in
    { re = a, im = p }

log10 : Complex -> Complex
log10 z = div (log z) { re = Basics.logBase Basics.e 10, im = 0 }

log2 : Complex -> Complex
log2 z = div (log z) { re = Basics.logBase Basics.e 2, im = 0 }

logBase : Complex -> Complex -> Complex
logBase x y = div (log x) (log y)

pow : Complex -> Complex -> Complex
pow a z =
  if a.im == 0 && z.im == 0 && a.re > 0 then
    { re = a.re ^ z.re, im = 0.0 }
  else
    let
      absa = abs a
      arga = arg a
      r = absa ^ z.re
      theta = z.re * absa
    in
      if absa == 0 then
        { re = 0, im = 0 }
      else if z.im == 0 then
        polar r theta
      else
        polar (r * (Basics.e ^ (negate z.im * arga))) (theta + z.im * Basics.logBase Basics.e absa)

pow2 : Complex -> Complex
pow2 z = mul z z

pow3 : Complex -> Complex
pow3 z = mul (mul z z) z

sqrt : Complex -> Complex
sqrt z =
  if z.re >= 0 then
    let
      t = Basics.sqrt ((z.re + rhypot z.re z.im ) * 0.5)
    in
      { re = t, im = 0.5 * z.im /t }
  else
    let
      t = Basics.sqrt ((negate z.re + rhypot z.re z.im ) * 0.5)
    in
      { re = 0.5 * Basics.abs z.im / t, im = rcopysign t z.im }

cbrt : Complex -> Complex
cbrt z =
  let
    r = (abs z) ^ (1.0/3.0)
    a = arg z
  in
    { re = r * Basics.cos (a/3), im = r * Basics.sin (a/3) }

rootn : Complex -> Complex -> Complex
rootn y x = pow y (inv x)

sqrtReal : Complex -> Complex
sqrtReal z =
  if z.im /= 0 then
    { re = nan, im = nan }
  else if z.re >= 0 then
    { re = Basics.sqrt z.re, im = 0.0 }
  else
    { re = nan, im = nan }


cbrtReal : Complex -> Complex
cbrtReal z =
  if z.im /= 0 then
    { re = nan, im = nan }
  else if z.re >= 0 then
    { re = z.re ^ (1.0/3.0), im = 0.0 }
  else
    { re = negate ((negate z.re) ^ (1.0/3.0)), im = 0.0 }

rootnReal : Complex -> Complex -> Complex
rootnReal z n =
  if z.im /= 0 then
    { re = nan, im = nan }
  else if n.im /= 0 then
    { re = nan, im = nan }
  else if n.re <= 0 then
    { re = nan, im = nan }
  else if Basics.toFloat (Basics.floor n.re) /= n.re then
    { re = nan, im = nan }
  else if z.re >= 0 then
    { re = z.re ^ (1.0 / n.re), im = 0.0 }
  else
    { re = negate ((negate z.re) ^ (1.0 / n.re)), im = 0.0 }



sin : Complex -> Complex
sin z =
  let
    chsh x =
      if Basics.abs(x) <= 0.5 then
        (rcosh x, rsinh x)
      else
        let
          e = Basics.e ^ x
          ei = 0.5 / e
        in
          (0.5 * e + ei , 0.5 * e - ei)
    (ch, sh) = chsh z.im
  in
    { re = ch * Basics.sin z.re, im = sh * Basics.cos z.re }

cos : Complex -> Complex
cos z =
  let
    chsh x =
      if Basics.abs(x) <= 0.5 then
        (rcosh x, rsinh x)
      else
        let
          e = Basics.e ^ x
          ei = 0.5 / e
        in
          (0.5 * e + ei , 0.5 * e - ei)
    (ch, sh) = chsh z.im
  in
    { re = ch * Basics.cos z.re, im = negate (sh * Basics.sin z.re) }

tan : Complex -> Complex
tan z =
  let
    w = tanh { re = negate z.im, im = z.re }
  in
    { re = w.im, im = negate w.re }

sinh : Complex -> Complex
sinh z =
  let
    w = sin { re = negate z.im, im = z.re }
  in
    { re = w.im, im = negate w.re }

cosh : Complex -> Complex
cosh z =cos { re = negate z.im, im = z.re }

tanh : Complex -> Complex
tanh z =
  let
    t = Basics.tan z.im
    beta = 1 + t * t
    s = rsinh z.re
    rho = Basics.sqrt (1.0 + s * s)
    denom = 1.0 + beta * s * s
  in
    { re = beta * rho * s, im = t / denom }

asin : Complex -> Complex
asin z =
  if z.im == 0 && Basics.abs z.re <= 1.0 then
    { re = Basics.asin z.re, im = 0.0 }
  else
    let
      z2 = mul z z
      w = log (add { re = negate z.im, im = z.re} (sqrt { re = 1.0 - z2.re, im = negate z2.im}))
    in
      { re = w.im, im = negate w.re }
      

acos : Complex -> Complex
acos z = 
  let
    w = asin z
  in
    { re = Basics.pi - w.re, im = negate w.im }

atan : Complex -> Complex
atan z =
  if z.im == 0 then
    { re = Basics.atan z.re, im = 0 }
  else
    let
      w = atanh { re = negate z.im, im = z.re }
    in
      { re = w.im, im = negate w.re }

asinh : Complex -> Complex
asinh z =
  let
    w = asin { re = negate z.im, im = z.re }
  in
    { re = w.im, im = negate w.re }

acosh : Complex -> Complex
acosh z =
  let
    w = acos z
  in
    { re = negate w.im, im = w.re }

atanh : Complex -> Complex
atanh z =
  let
    x = log ( div { re = 1 + z.re, im = z.im } { re = 1 - z.re, im = negate z.im } )
  in
    { re = rcopysign x.re z.re, im = rcopysign x.im z.im } 



degree2radian : Complex -> Complex
degree2radian z = let a = Basics.pi / 180.0 in { re = a * z.re, im = a * z.im }

radian2degree : Complex -> Complex
radian2degree z = let a = 180 / Basics.pi in { re = a * z.re, im = a * z.im }


-- ----------------------------------
-- math functions of real numbers
-- ----------------------------------

rexpm1 : Float -> Float
rexpm1 x =
  let
    work n y = if n > 16 then 1.0 else (y / toFloat n) * (1.0 + work (n + 1) y)
  in
    if Basics.abs x < 1.0 then work 1 x else Basics.e ^ x - 1

rsinh : Float -> Float
rsinh x = if x < 0 then negate (rsinh (negate x)) else -0.5 * Basics.e ^ x * rexpm1 (-2.0 * x)

rcosh : Float -> Float
rcosh x = if x < 0 then rcosh (-x) else 0.5 * Basics.e ^ x * (1.0 + Basics.e ^ (-2.0 * x))

rhypot : Float -> Float -> Float
rhypot x y =
  let
    work c d =
      if c == 0 then
        0
      else
        c * Basics.sqrt (1.0 + d*d / (c*c))
    a = Basics.abs x
    b = Basics.abs y
  in
    if a > b then work a b else work b a

rcopysign : Float -> Float -> Float
rcopysign x y = if y < 0 then Basics.abs x else negate (Basics.abs x)

-- ----------------------------------
-- formatter
-- ----------------------------------

type alias FormatType = { formatExp : Bool, decimalComma : Bool } 

type FormatHMSType = FormatHH | FormatHMM | FormatHMS

formatHMS : FormatType -> FormatHMSType -> Complex -> String
formatHMS fmt hmstype z =
  let
    format = formatFloat fmt
    trunc = toFloat << truncate
    h = z.re
    m = Basics.abs (h - (trunc h)) * 60.0
    s = Basics.abs (m - (trunc m)) * 60.0
  in
    case hmstype of
      FormatHH -> format h
      FormatHMM -> format (trunc h) ++ ":" ++ format m
      FormatHMS -> format (trunc h) ++ ":" ++ format (trunc m) ++ ":" ++ format s


formatComplex : FormatType -> Complex -> String
formatComplex fmt z =
  if isNaN z.re || isNaN z.im then
    "NaN"
  else if z.re == 0 && z.im == 0 then
    "0"
  else if z.re == 0 then
    formatFloat fmt z.im ++ "i"
  else if z.im == 0 then
    formatFloat fmt z.re
  else if z.im < 0 then
    formatFloat fmt z.re ++ " " ++ formatFloat fmt z.im ++ "i"
  else
    formatFloat fmt z.re ++ " +" ++ formatFloat fmt z.im ++ "i"



formatFloat : FormatType -> Float -> String
formatFloat fmt x =
  if isNaN x then
    "NaN"
  else if isInfinite x then
    if x < 0 then "-inf" else "inf"
  else if x == 0 then
    "0"
  else if x < 0 then
    "-" ++ formatPositiveFloat fmt (-x)
  else
    formatPositiveFloat fmt x
 
formatPositiveFloat : FormatType -> Float -> String
formatPositiveFloat fmt x =
  let
    e = Basics.floor (Basics.logBase 10.0 x)
    s = x / toFloat (10 ^ e)
  in
    if fmt.formatExp then
      formatPositiveFloatExp fmt 10 s e
    else
      formatPositiveFloatFixRound fmt 10 s e

formatPositiveFloatExp fmt n s e =
  let
    epart = if e == 0 then "" else "e" ++ String.fromInt e
  in
    formatPositiveFloatFixRound fmt n s 0 ++ epart

formatPositiveFloatFixRound fmt n s e =
  let
    str = formatPositiveFloatFix fmt n s e
    sep = if fmt.decimalComma then ',' else '.'
  in
    if hasSeparator sep str then
      removeTrailingZeros sep <| formatRoundPositiveFloat str
    else
      str

removeTrailingZeros sep s =
  let
    work xx = case String.uncons xx of
      Nothing -> ""
      Just (x, xs) ->
        if x == '0' then
          work xs
        else if x == sep then
          xs
        else
          xx
  in
    String.reverse (work (String.reverse s))



formatPositiveFloatFix fmt n s e =
  let
    a = Basics.floor s
    b = (s - toFloat a) * 10.0
    sep = if fmt.decimalComma then "," else "."
  in
    if n == 0 then
      String.repeat e "0"
    else if e < 0 then
      "0" ++ sep ++ String.repeat (-e-1) "0" ++ formatFloatDigits n s
    else if e == 0 then
      String.fromInt a ++ sep ++ formatFloatDigits (n-1) b
    else
      String.fromInt a ++ formatPositiveFloatFix fmt (n-1) b (e-1)


hasSeparator sep s = String.any (\c -> c == sep) s


formatFloatDigits n s =
  if n == 0 then
    ""
  else
    let
      a = Basics.floor s
      b = (s - toFloat a) * 10.0
    in
      String.fromInt a ++ formatFloatDigits (n-1) b


formatRoundPositiveFloat s =
  let
    roundUp xx = case String.uncons xx of
      Nothing -> "1"
      Just (x, xs) ->
        if x == '9' then
          "0" ++ roundUp xs
        else if x == '8' then
          "9" ++ xs
        else if x == '7' then
          "8" ++ xs
        else if x == '6' then
          "7" ++ xs
        else if x == '5' then
          "6" ++ xs
        else if x == '4' then
          "5" ++ xs
        else if x == '3' then
          "4" ++ xs
        else if x == '2' then
          "3" ++ xs
        else if x == '1' then
          "2" ++ xs
        else if x == '0' then
          "1" ++ xs
        else
          String.cons x (roundUp xs)
  in case String.uncons (String.reverse s) of
    Nothing -> ""
    Just (y, ys) ->
      if y == '0' || y == '1' || y == '2' || y == '3' || y == '4' then
        String.reverse ys
      else
        String.reverse (roundUp ys)


-- parser

parseNumber : String -> String -> Maybe Complex
parseNumber sep str =
  case String.split ":" str of
    [_] -> parseDecNumber sep str
    [h,m] -> parseHMM sep h m
    [h,m,s] -> parseHMS sep h m s
    _ -> Nothing

parseHMM sep hstr mstr = case (parseRealExp sep hstr, parseRealExp sep mstr) of
  (Just h, Just m) -> Just { re = (h + m / 60.0), im = 0.0 }
  _ -> Nothing

parseHMS sep hstr mstr sstr = case (parseRealExp sep hstr, parseRealExp sep mstr, parseRealExp sep sstr) of
  (Just h, Just m, Just s) -> Just { re = h + m / 60.0 + s / 3600.0, im = 0.0 }
  _ -> Nothing



parseDecNumber : String -> String -> Maybe Complex
parseDecNumber sep str =
  case String.indices "i" str of
    []       -> Maybe.map (\x -> { re = x, im = 0 }) (parseRealExp sep str)
    [iIndex] ->
      let
        realStr = String.left iIndex str
        imagStr = String.dropLeft (iIndex + 1) str
        realPart =
          if String.isEmpty realStr then
            Just 0.0
          else
            parseRealExp sep realStr
        imagPart =
          if String.isEmpty imagStr then
            Just 1.0
          else
            parseRealExp sep imagStr
      in
        case (realPart, imagPart) of
          (Just r, Just i) -> Just { re = r, im = i }
          _                -> Nothing
    _ -> Nothing

parseRealExp : String -> String -> Maybe Float
parseRealExp sep str =
  case String.indices "e" str of
    [eIndex] ->
      let
        sigStr = String.left eIndex str
        expStr = String.dropLeft (eIndex + 1) str
        sigPart =
          if String.isEmpty sigStr then
            Just 1.0
          else if sigStr == "-" then
            Just 1.0
          else
            parseRealDot sep sigStr
        expPart =
          if String.isEmpty expStr then
            Just 0
          else if expStr == "-" then
            Just 0
          else
            parseSignedInteger expStr
      in
        case (sigPart, expPart) of
          (Just s, Just e) -> Just (s * (10 ^ e))
          _                -> Nothing
    []       -> parseRealDot sep str
    _ -> Nothing

parseRealDot sep str =
  case String.indices sep str of
    [dotIndex] ->
      let
        preStr = String.left dotIndex str
        postStr = String.dropLeft (dotIndex + 1) str
        pre = parseSignedInteger preStr
        post = parseInteger postStr
        shift = 0.1 ^ (toFloat (String.length postStr))
      in
        case (pre, post) of
          (Just preN, Just postN) -> Just (if preN < 0 then preN - postN * shift else preN + postN * shift)
          _ -> Nothing
    [] -> parseSignedInteger str
    _ -> Nothing

parseSignedInteger : String -> Maybe Float 
parseSignedInteger str =
  if String.startsWith "-" str then
    parseInteger (String.dropLeft 1 str) |> Maybe.map negate
  else
    parseInteger str

parseInteger : String -> Maybe Float
parseInteger s =
  if String.all Char.isDigit s then
    Maybe.map Basics.toFloat <| String.toInt s
  else
    Nothing
