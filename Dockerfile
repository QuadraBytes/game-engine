# =========================
# Build stage
# =========================
FROM haskell:9.10 AS builder

WORKDIR /app

# Copy stack config and cabal files first
COPY stack.yaml stack.yaml.lock package.yaml game-engine.cabal ./

# Tell stack NOT to use system GHC
RUN stack config set system-ghc --global false

# Install correct GHC + dependencies
RUN stack setup --install-ghc
RUN stack build --only-dependencies --install-ghc

# Copy source files
COPY . .

# Build executable
RUN stack build --copy-bins --local-bin-path /app/bin --install-ghc


# =========================
# Runtime stage
# =========================
FROM debian:bookworm-slim

WORKDIR /app

RUN apt-get update && apt-get install -y \
    libgmp10 \
    ca-certificates \
 && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/bin/game-engine /app/game-engine

EXPOSE 3000

CMD ["./game-engine"]
