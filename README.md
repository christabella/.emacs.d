# .emacs.d
![Current setup](screenshots/org-magit-and-code.png "Current setup")
![Org mode in fullscreen](screenshots/org.png "Org mode")

Very much borrowed from and inspired by [Jethro Kuan's Emacs config](https://github.com/jethrokuan/.emacs.d).

# Loved packages :heart_eyes_cat:
- [`magit`](https://magit.vc) for Git magic :sparkles:
- [`org-download`](https://github.com/abo-abo/org-download/) for inserting images into `org` files.
- [`org-journal`](https://github.com/bastibe/org-journal) for easy journalling.
- [`ox-hugo`](https://github.com/kaushalmodi/ox-hugo) for exporting [my Hugo blogposts](https://bella.cc/blog).
- [`ox-reveal`](https://github.com/hexmode/ox-reveal) for exporting [my Reveal.js presentations](https://bella.cc/docs/neural_processes).
- [`emojify`](https://github.com/iqbalansari/emacs-emojify) because :100: :curry:
- [`nyan-mode`](https://github.com/TeMPOraL/nyan-mode) because :rainbow:
- and many more, for sure!

# Loved themes :nail_care:
## Dark
- `doom-dracula`
- `kaolin-ocean`
- `doom-nord`
##  Light
- `kaolin-breeze`
- `kaolin-light`

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
## For `pdf-tools`'
```
brew tap dunn/emacs
brew install --HEAD pdf-tools
```
