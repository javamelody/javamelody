# JavaMelody Spring Boot Starter

Spring Boot Starter to facilitate the integration of JavaMelody in Spring Boot web applications.

See the [example project](../javamelody-for-spring-boot) for a demonstration.

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
      # Enable JavaMelody auto-configuration (optional, default: true)
      enabled: true
      # Data source names to exclude from monitoring (optional, comma-separated)
      excluded-datasources: secretSource,topSecretSource
      # Enable monitoring of Spring services and controllers (optional, default: true)
      spring-monitoring-enabled: true
      # Initialization parameters for JavaMelody (optional)
      # See: https://github.com/javamelody/javamelody/wiki/UserGuide#6-optional-parameters
      init-parameters:
        # log http requests
        log: true
        # to exclude images, css, fonts and js urls from the monitoring:
        #url-exclude-pattern: (/webjars/.*|/css/.*|/images/.*|/fonts/.*|/js/.*)
        # to add basic auth:
        #authorized-users: admin:pwd
        # to change the default storage directory:
        #storage-directory: /tmp/javamelody
        # to change the default "/monitoring" path
        #monitoring-path: /admin/performance


Example for `application.properties`:

    # Enable JavaMelody auto-configuration (optional, default: true)
    javamelody.enabled=true
    # Data source names to exclude from monitoring (optional, comma-separated)
    javamelody.excluded-datasources=secretSource,topSecretSource
    # Enable monitoring of Spring services and controllers (optional, default: true)
    javamelody.spring-monitoring-enabled=true
    # Initialization parameters for JavaMelody (optional)
    # See: https://github.com/javamelody/javamelody/wiki/UserGuide#6-optional-parameters
    #    log http requests
    javamelody.init-parameters.log=true
    #    to exclude images, css, fonts and js urls from the monitoring:
    # javamelody.init-parameters.url-exclude-pattern: (/webjars/.*|/css/.*|/images/.*|/fonts/.*|/js/.*)
    #    to add basic auth:
    # javamelody.init-parameters.authorized-users: admin:pwd
    #    to change the default storage directory:
    # javamelody.init-parameters.storage-directory: /tmp/javamelody
    #    to change the default "/monitoring" path
    # javamelody.init-parameters.monitoring-path=/admin/performance

## License

[ASL](http://www.apache.org/licenses/LICENSE-2.0)
