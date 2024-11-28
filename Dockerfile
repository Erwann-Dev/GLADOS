# Use the official Haskell image from the Docker Hub
FROM haskell:latest

# Set the working directory
WORKDIR /usr/src/app

# Copy the current directory contents into the container at /usr/src/app
COPY . .

# Install stack if not already installed
RUN curl -sSL https://get.haskellstack.org/ | sh -s - -f

# Install any needed packages specified in the stack.yaml file
RUN stack setup --install-ghc
RUN stack build --only-dependencies

# Build the Haskell program using Makefile
RUN make

# Run the Haskell program using Makefile
CMD ["make", "run"]
