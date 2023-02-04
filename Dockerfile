FROM openjdk:8-alpine

COPY target/uberjar/puzzlesnap.jar /puzzlesnap/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/puzzlesnap/app.jar"]
