# ---- Build stage: official JDK + sbt installed explicitly
FROM eclipse-temurin:17-jdk AS build
ARG SBT_VERSION=1.10.7
RUN apt-get update \
 && apt-get install -y --no-install-recommends curl ca-certificates \
 && curl -fsSL "https://github.com/sbt/sbt/releases/download/v${SBT_VERSION}/sbt-${SBT_VERSION}.tgz" \
    | tar -xz -C /opt \
 && rm -rf /var/lib/apt/lists/*
ENV PATH="/opt/sbt/bin:${PATH}"
WORKDIR /src
COPY . .
RUN sbt server/assembly

# ---- Runtime stage: only a JRE and the JAR
FROM eclipse-temurin:17-jre
WORKDIR /app
COPY --from=build /src/server/target/scala-3.3.5/scalajack-server.jar /app/server.jar
EXPOSE 8080
# NODE_ROLE (lobby | table) is deliberately not set here: it is chosen by Compose.
ENTRYPOINT ["java", "-jar", "/app/server.jar"]