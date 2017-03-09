package net.bull.javamelody;

import java.util.HashMap;
import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = JavaMelodyConfigurationProperties.PREFIX)
public class JavaMelodyConfigurationProperties {
  public static final String PREFIX = "javamelody";

  private boolean enabled = true;
  private String applicationType;
  private String filterName;
  private Map<String, String> initParameters = new HashMap<String, String>();

  public boolean isEnabled() {
    return enabled;
  }

  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  public String getApplicationType() {
    return applicationType;
  }

  public void setApplicationType(String applicationType) {
    this.applicationType = applicationType;
  }

  public String getFilterName() {
    return filterName;
  }

  public void setFilterName(String filterName) {
    this.filterName = filterName;
  }

  public Map<String, String> getInitParameters() {
    return initParameters;
  }

  public void setInitParameters(Map<String, String> initParameters) {
    this.initParameters = initParameters;
  }

  @Override
  public String toString() {
    return "JavaMelodyConfigurationProperties [enabled=" + enabled + ", applicationType=" + applicationType
        + ", filterName=" + filterName + ", initParameters=" + initParameters + "]";
  }

}
