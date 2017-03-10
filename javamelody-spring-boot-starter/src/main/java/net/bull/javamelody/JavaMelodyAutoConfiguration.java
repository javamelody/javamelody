package net.bull.javamelody;

import java.util.Map.Entry;

import javax.servlet.DispatcherType;

import org.springframework.aop.framework.autoproxy.DefaultAdvisorAutoProxyCreator;
import org.springframework.aop.support.annotation.AnnotationMatchingPointcut;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.condition.ConditionalOnWebApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.stereotype.Controller;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.RestController;

/**
 * Spring Boot auto-configuration for JavaMelody.
 *
 * <p>
 * This class is picked up by the Spring Boot auto-configuration mechanism and creates the beans required to set up
 * JavaMelody. The monitoring filter is created with the name "javamelody" and the application type "Spring Boot".
 * Configuration values are injected using {@link JavaMelodyConfigurationProperties}.
 * </p>
 *
 * <p>
 * The auto-configured filter can be overridden by defining a custom {@link FilterRegistrationBean} with the name
 * "javamelody-registration" in the application context.
 * </p>
 *
 * <p>
 * The configuration is enabled for web applications by default. It is possible to opt-out of the auto-configuration by
 * setting the application configuration "javamelody.enabled" to the value "false".
 * </p>
 *
 * @author Georg Wittberger
 * @since 1.64.0
 */
@Configuration
@EnableConfigurationProperties(JavaMelodyConfigurationProperties.class)
@ConditionalOnWebApplication
@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "enabled", matchIfMissing = true)
public class JavaMelodyAutoConfiguration {
  public static final String REGISTRATION_BEAN_NAME = "javamelody-registration";
  public static final String DEFAULT_FILTER_NAME = "javamelody";
  public static final String DEFAULT_APPLICATION_TYPE = "Spring Boot";

  /**
   * Registers the JavaMelody session listener.
   */
  @Bean
  public SessionListener monitoringSessionListener() {
    return new SessionListener();
  }

  /**
   * Registers the JavaMelody monitoring filter. The filter can be overridden completely by creating a custom
   * {@link FilterRegistrationBean} with the name "javamelody-registration" in the application context.
   */
  @Bean(name = REGISTRATION_BEAN_NAME)
  @ConditionalOnMissingBean(name = REGISTRATION_BEAN_NAME)
  public FilterRegistrationBean monitoringFilter(JavaMelodyConfigurationProperties properties) {
    FilterRegistrationBean registrationBean = new FilterRegistrationBean();

    // Create the monitoring filter and set its configuration parameters.
    MonitoringFilter filter = new MonitoringFilter();
    filter.setApplicationType(DEFAULT_APPLICATION_TYPE);

    // Wrap the monitoring filter in the registration bean.
    registrationBean.setFilter(filter);
    registrationBean.setAsyncSupported(true);
    registrationBean.setName(DEFAULT_FILTER_NAME);
    registrationBean.setDispatcherTypes(DispatcherType.REQUEST, DispatcherType.ASYNC);

    // Set the initialization parameter for the monitoring filter.
    for (Entry<String, String> parameter : properties.getInitParameters().entrySet()) {
      registrationBean.addInitParameter(parameter.getKey(), parameter.getValue());
    }

    // Set the URL patterns to activate the monitoring filter for.
    registrationBean.addUrlPatterns("/*");
    return registrationBean;
  }

  @Bean
  @ConditionalOnMissingBean(DefaultAdvisorAutoProxyCreator.class)
  public DefaultAdvisorAutoProxyCreator defaultAdvisorAutoProxyCreator() {
    return new DefaultAdvisorAutoProxyCreator();
  }

  /**
   * Monitoring of JDBC Data Sources
   */
  @Bean
  public SpringDataSourceBeanPostProcessor monitoringDataSourceBeanPostProcessor() {
    SpringDataSourceBeanPostProcessor processor = new SpringDataSourceBeanPostProcessor();
    processor.setExcludedDatasources(null);
    return processor;
  }

  /**
   * Monitoring of beans and methods having the {@link MonitoredWithSpring} annotation.
   */
  @Bean
  public MonitoringSpringAdvisor monitoringSpringAdvisor() {
    final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();
    interceptor.setPointcut(new MonitoredWithAnnotationPointcut());
    return interceptor;
  }

  /**
   * Monitoring of beans having the {@link Service} annotation.
   */
  @Bean
  public MonitoringSpringAdvisor monitoringSpringServiceAdvisor() {
    final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();
    interceptor.setPointcut(new AnnotationMatchingPointcut(Service.class));
    return interceptor;
  }

  /**
   * Monitoring of beans having the {@link Controller} annotation.
   */
  @Bean
  public MonitoringSpringAdvisor monitoringSpringControllerAdvisor() {
    final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();
    interceptor.setPointcut(new AnnotationMatchingPointcut(Controller.class));
    return interceptor;
  }

  /**
   * Monitoring of beans having the {@link RestController} annotation.
   */
  @Bean
  public MonitoringSpringAdvisor monitoringSpringRestControllerAdvisor() {
    final MonitoringSpringAdvisor interceptor = new MonitoringSpringAdvisor();
    interceptor.setPointcut(new AnnotationMatchingPointcut(RestController.class));
    return interceptor;
  }
}
