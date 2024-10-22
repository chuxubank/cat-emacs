# syntax = docker/dockerfile:1.2
FROM silex/emacs as builder
USER root

RUN apt-get -y update && apt-get install -y \
    aspell-en \
    elpa-pdf-tools-server \
    gh \
    git

ADD . /root/.emacs.d

RUN echo "(custom-set-variables '(use-short-answers t) '(package-native-compile t) '(system-packages-use-sudo nil))" > /root/.emacs.d/custom.el

RUN --mount=type=cache,sharing=locked,target=/root/.emacs.d/elpa \
    yes | emacs --fg-daemon --debug-init --eval "(kill-emacs)"

RUN emacs --batch -f batch-byte-recompile-directory /root/.emacs.d/

CMD ["emacs"]
