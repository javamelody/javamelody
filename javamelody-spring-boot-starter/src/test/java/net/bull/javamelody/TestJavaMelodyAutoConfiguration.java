package net.bull.javamelody;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Map;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.aop.framework.autoproxy.DefaultAdvisorAutoProxyCreator;
import org.springframework.aop.support.annotation.AnnotationMatchingPointcut;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.context.web.WebAppConfiguration;

import net.bull.javamelody.TestJavaMelodyAutoConfiguration.TestContextConfiguration;

@RunWith(SpringRunner.class)
@ContextConfiguration(classes = TestContextConfiguration.class)
@WebAppConfiguration
@TestPropertySource(properties = { "javamelody.init-parameters.log=true",
    "javamelody.init-parameters.monitoring-path=/test/path", "javamelody.excluded-datasources=ds1,ds2" })
public class TestJavaMelodyAutoConfiguration {
  @Configuration
  @EnableAutoConfiguration
  static class TestContextConfiguration {
  }

  @Autowired
  private ApplicationContext context;

  @Test
  public void testJavaMelodyAutoConfigurationIsCreated() {
    // It should create a session listener.
    SessionListener sessionListener = context.getBean(SessionListener.class);
    assertThat(sessionListener).isNotNull();

    // It should create a registration bean named "javamelody-registration".
    Object registrationBean = context.getBean(JavaMelodyAutoConfiguration.REGISTRATION_BEAN_NAME);
    assertThat(registrationBean).isNotNull();
    assertThat(registrationBean).isInstanceOf(FilterRegistrationBean.class);

    // It should create a filter registration bean with the appropriately configured monitoring filter.
    FilterRegistrationBean filterRegistrationBean = (FilterRegistrationBean) registrationBean;
    assertThat(filterRegistrationBean.getFilter()).isNotNull();
    assertThat(filterRegistrationBean.getFilter()).isInstanceOf(MonitoringFilter.class);
    assertThat(filterRegistrationBean.getInitParameters()).containsEntry("log", "true");
    assertThat(filterRegistrationBean.getInitParameters()).containsEntry("monitoring-path", "/test/path");
    assertThat(filterRegistrationBean.getUrlPatterns()).containsExactly("/*");

    // It should create the monitoring filter with the application type "Spring Boot".
    MonitoringFilter monitoringFilter = (MonitoringFilter) filterRegistrationBean.getFilter();
    assertThat(monitoringFilter.getApplicationType()).isEqualTo(JavaMelodyAutoConfiguration.DEFAULT_APPLICATION_TYPE);

    // It should create an auto-proxy creator.
    DefaultAdvisorAutoProxyCreator autoProxyCreator = context.getBean(DefaultAdvisorAutoProxyCreator.class);
    assertThat(autoProxyCreator).isNotNull();

    // It should create a bean post-processor for data sources.
    SpringDataSourceBeanPostProcessor dataSourcePostProcessor = context
        .getBean(SpringDataSourceBeanPostProcessor.class);
    assertThat(dataSourcePostProcessor).isNotNull();
    // Cannot test excluded data sources since there is no public accessor for that.

    // It should create interceptors to monitor Spring services and controllers.
    Map<String, MonitoringSpringAdvisor> springAdvisors = context.getBeansOfType(MonitoringSpringAdvisor.class);
    boolean monitoredWithAdvisorFound = false;
    int stereotypeAdvisorsCount = 0;
    for (MonitoringSpringAdvisor springAdvisor : springAdvisors.values()) {
      if (springAdvisor.getPointcut() instanceof MonitoredWithAnnotationPointcut) {
        monitoredWithAdvisorFound = true;
      } else if (springAdvisor.getPointcut() instanceof AnnotationMatchingPointcut) {
        stereotypeAdvisorsCount++;
        // Maybe use synthetic @Service, @Controller and @RestController classes to check if point cuts match.
      }
    }
    assertThat(monitoredWithAdvisorFound).isTrue();
    assertThat(stereotypeAdvisorsCount).isEqualTo(3);
  }
}
