# Reverse Worldle

## Setup

### Development

```
npx elm-watch hot
```

then go to `reverse-wordle/index.html` in the browser.

### Testing

```
elm-test
```

or

```
elm-test --watch --fuzz 10
```

### Compile

```
elm make src/ReverseWordle.elm --optimize --output=dist/main.js
npm run uglify
sed 's/main.js/main.min.js/' index.html > dist/index.html
cp main.css dist/main.css
```

