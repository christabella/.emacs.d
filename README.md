# .emacs.d
![Current setup](screenshots/org-magit-and-code.png "Current setup")

# Emacs installation
```
brew install emacs --with-cocoa --with-gnutls --with-dbus --with-librsvg --with-imagemagick@6 --with-mailutils --devel
```
In particular, we need ImageMagick so that `org-image-actual-width` will work as expected.

# Non-Emacs setup
## For `flyspell`
```
brew install ispell --with-lang-en
```

