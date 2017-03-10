# JavaMelody for Spring Boot Example

This example project demonstrates how to integrate JavaMelody in a Spring Boot web application using our starter package.

There are only two essential things to add to any basic Spring Boot project:

1.  Adding the dependency `net.bull.javamelody:javamelody-spring-boot-starter` to the POM of the application. See [pom.xml](../../../blob/master/javamelody-for-spring-boot/pom.xml).
2.  Adding custom configuration for JavaMelody to the `application.yml` or `application.properties`. See [src/main/resources/application.yml](../../../blob/master/javamelody-for-spring-boot/src/main/resources/application.yml).

Run the example application with Maven using `mvn spring-boot:run`. Then go to: http://localhost:8080/monitoring
