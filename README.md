# .emacs.d
![Current setup](screenshots/org-magit-and-code.png "Current setup")
![Org mode in fullscreen](screenshots/org.png "Org mode")
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

