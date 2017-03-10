package net.bull.javamelody;

import java.util.HashMap;
import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Configuration properties for JavaMelody. This class is used for binding the configuration values in "application.yml"
 * or "application.properties".
 *
 * @author Georg Wittberger
 * @since 1.64.0
 */
@ConfigurationProperties(prefix = JavaMelodyConfigurationProperties.PREFIX)
public class JavaMelodyConfigurationProperties {
  public static final String PREFIX = "javamelody";

  private boolean enabled = true;
  private Map<String, String> initParameters = new HashMap<String, String>();

  /**
   * Returns if JavaMelody should be enabled within the application.
   *
   * @return <code>true</code> if JavaMelody should be enabled, otherwise <code>false</code>. Default: <code>true</code>
   */
  public boolean isEnabled() {
    return enabled;
  }

  /**
   * Sets whether JavaMelody should be enabled within the application.
   *
   * @param enabled <code>true</code> if JavaMelody should be enabled, otherwise <code>false</code>.
   */
  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
  }

  /**
   * Returns a map of initialization parameters to be passed to the JavaMelody monitoring filter.
   *
   * @return Initialization parameters for the JavaMelody monitoring filter.
   */
  public Map<String, String> getInitParameters() {
    return initParameters;
  }

  /**
   * Sets a map of initialization parameters to be passed to the JavaMelody monitoring filter.
   *
   * @param initParameters Initialization parameters for the JavaMelody monitoring filter.
   */
  public void setInitParameters(Map<String, String> initParameters) {
    this.initParameters = initParameters;
  }

  @Override
  public String toString() {
    return "JavaMelodyConfigurationProperties [enabled=" + enabled + ", initParameters=" + initParameters + "]";
  }
}
