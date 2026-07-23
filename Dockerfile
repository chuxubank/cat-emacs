# syntax = docker/dockerfile:1.2
ARG EMACS_PLATFORM=linux/amd64
FROM --platform=$EMACS_PLATFORM silex/emacs AS base
ARG CI
ENV CI=$CI

RUN --mount=type=cache,sharing=locked,target=/var/cache/apt \
    --mount=type=cache,sharing=locked,target=/var/lib/apt \
    apt-get -y update && apt-get --no-install-recommends install -y \
    aspell-en \
    elpa-pdf-tools-server \
    gh \
    git \
    make \
    p7zip \
    tzdata

WORKDIR /root/.config/emacs

FROM base AS manifest

COPY . .

RUN --mount=type=cache,id=emacs-packages,sharing=locked,target=/root/.config/emacs/elpa \
    --mount=type=cache,id=emacs-eln-cache,sharing=locked,target=/root/.config/emacs/eln-cache \
    XDG_CONFIG_HOME=/tmp/cat-emacs-config \
    make package-manifest \
    PACKAGE_MANIFEST=/tmp/cat-emacs-package-manifest.eld

FROM base AS packages
ARG PACKAGE_CACHE_EPOCH

COPY early-init.el Makefile ./
COPY core/package/archives.el core/package/manifest.el ./core/package/
COPY --from=manifest /tmp/cat-emacs-package-manifest.eld /tmp/cat-emacs-package-manifest.eld

RUN --mount=type=cache,id=emacs-packages,sharing=locked,target=/root/.config/emacs/elpa \
    --mount=type=cache,id=emacs-eln-cache,sharing=locked,target=/root/.config/emacs/eln-cache \
    echo "Package cache epoch: ${PACKAGE_CACHE_EPOCH:-manual}" && \
    make sync-package-manifest \
      PACKAGE_MANIFEST=/tmp/cat-emacs-package-manifest.eld && \
    make compile-org && \
    mkdir -p /opt/cat-emacs && \
    cp -a elpa /opt/cat-emacs/elpa && \
    cp -a eln-cache /opt/cat-emacs/eln-cache

FROM base AS final

COPY --from=packages /opt/cat-emacs/elpa ./elpa
COPY --from=packages /opt/cat-emacs/eln-cache ./eln-cache
COPY . .

ENTRYPOINT ["emacs"]
