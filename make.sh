set -eu

OUT=index.html
TITLE='ARSCalc'

elm make src/Main.elm --output=index.js

rm -f $OUT

cat >> $OUT <<EOS
<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8"/>
<meta name="viewport" content="width=device-width,initial-scale=1.0,minimum-scale=1.0">
<title>$TITLE</title>

<style> body { padding: 0; margin: 0; } button:active { border-color: #804000; } </style>
</head>
<body>
<pre id="elm"></pre>
<script type='text/javascript'>
EOS


#npx terser --compress --mangle -- index.js >> $OUT
cat index.js >> $OUT

cat >> $OUT <<EOS

var cookies = '';
if (typeof(document.cookie) !== "undefined") { cookies = document.cookie + ';' };
var settingVal = (cookies.match(/settings=([^;]*)/)||['',''])[1];
var seedArray = new Uint32Array(1);
window.crypto.getRandomValues(seedArray);

var app = Elm.Main.init({
  flags: { seed : seedArray[0], settings : settingVal },
  node: document.getElementById("elm")
});

app.ports.saveSettingsToCookie.subscribe(function(settings){
  document.cookie = 'settings=' + settings + '; SameSite=strict';
})

</script>
</body>
</html>
EOS

