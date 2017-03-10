# JavaMelody Spring Boot Starter

Spring Boot Starter to facilitate the integration of JavaMelody in Spring Boot web applications.

See the [example project](https://github.com/javamelody/javamelody/tree/master/javamelody-for-spring-boot) for a demonstration.

## Integration

Simply add the following dependency to your Maven POM:

    <dependency>
      <groupId>net.bull.javamelody</groupId>
      <artifactId>javamelody-spring-boot-starter</artifactId>
      <version>1.64.0-SNAPSHOT</version>
    </dependency>

Optionally add the iText dependency if you want to use PDF exports:

    <dependency>
      <groupId>com.lowagie</groupId>
      <artifactId>itext</artifactId>
      <version>2.1.7</version>
      <exclusions>
        <exclusion>
          <artifactId>bcmail-jdk14</artifactId>
          <groupId>bouncycastle</groupId>
        </exclusion>
        <exclusion>
          <artifactId>bcprov-jdk14</artifactId>
          <groupId>bouncycastle</groupId>
        </exclusion>
        <exclusion>
          <artifactId>bctsp-jdk14</artifactId>
          <groupId>bouncycastle</groupId>
        </exclusion>
      </exclusions>
    </dependency>

## Configuration

Use the configuration properties prefixed with `javamelody` in your `application.yml` or `application.properties`.

Example for `application.yml`:

    javamelody:
      # Enable JavaMelody auto-configuration (default: true)
      enabled: true
      # Optional initialization parameters for JavaMelody
      # See: https://github.com/javamelody/javamelody/wiki/UserGuide#6-optional-parameters
      init-parameters:
        log: true
        monitoring-path: /admin/performance

Example for `application.properties`:

    # Enable JavaMelody auto-configuration (default: true)
    javamelody.enabled=true
    # Optional initialization parameters for JavaMelody
    # See: https://github.com/javamelody/javamelody/wiki/UserGuide#6-optional-parameters
    javamelody.init-parameters.log=true
    javamelody.init-parameters.monitoring-path=/admin/performance

## License

[ASL](http://www.apache.org/licenses/LICENSE-2.0)
