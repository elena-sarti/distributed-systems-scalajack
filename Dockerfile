# ---- Build stage: official JDK + sbt installed explicitly
FROM eclipse-temurin:17-jdk AS build
ARG SBT_VERSION=1.11.3
RUN apt-get update \
 && apt-get install -y --no-install-recommends curl ca-certificates \
 && curl -fsSL "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" \
    | tar -xz -C /opt \
 && rm -rf /var/lib/apt/lists/*
ENV PATH="/opt/sbt/bin:${PATH}"
WORKDIR /src
# Only what the server needs. The libraries stay in a cache of Docker between builds,
# so a change in the code does not download them again.
COPY build.sbt ./
COPY project/build.properties project/plugins.sbt project/
COPY model model
COPY protocol protocol
COPY server server
RUN --mount=type=cache,target=/root/.cache/coursier --mount=type=cache,target=/root/.sbt \
    sbt server/assembly

# ---- Runtime stage: only a JRE and the JAR
FROM eclipse-temurin:17-jre
# curl answers the health checks of docker-compose.yml
RUN apt-get update \
 && apt-get install -y --no-install-recommends curl \
 && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY --from=build /src/server/target/scala-3.3.5/scalajack-server.jar /app/server.jar
EXPOSE 8080 8090
# Up to eight nodes run on one machine: each keeps a small heap. JAVA_OPTS can change it.
ENV JAVA_OPTS="-Xmx256m"
# NODE_ROLE (lobby | table | backup) is deliberately not set here: it is chosen by Compose.
# exec: the JVM gets the signals of `docker stop`, so the nodes shut down cleanly.
ENTRYPOINT ["sh", "-c", "exec java $JAVA_OPTS -jar /app/server.jar"]
