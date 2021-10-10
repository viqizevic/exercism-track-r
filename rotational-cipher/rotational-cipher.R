rotate <- function(text, key) {
  # Note the upper case letters
  upc <- utf8ToInt(text) == utf8ToInt(toupper(text))
  # Coding for 'a'
  ua <- utf8ToInt('a')
  # Get coding for the letters in lower case
  v <- utf8ToInt(tolower(text)) - ua
  # Add the rotation
  x <- ((v + key) %% 26) + ua
  # Set the upper case letters
  x[upc] <- x[upc] - ua + utf8ToInt('A')
  # Revert the non-letters
  x[v<0] <- v[v<0] + ua
  # Get back as letters
  intToUtf8(x)
}