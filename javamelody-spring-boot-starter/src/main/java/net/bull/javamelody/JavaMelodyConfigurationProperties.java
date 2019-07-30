/*
 * Copyright 2008-2019 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.bull.javamelody;

import java.util.HashMap;
import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Configuration properties for JavaMelody. <br/>
 * This class is used for binding the configuration values in "application.yml" or "application.properties".
 *
 * @author Georg Wittberger
 * @since 1.64.0
 */
@ConfigurationProperties(prefix = JavaMelodyConfigurationProperties.PREFIX)
public class JavaMelodyConfigurationProperties {
	/**
	 * Prefix of properties names.
	 */
	public static final String PREFIX = "javamelody";

	/**
	 * If JavaMelody should be enabled within the application.
	 */
	private boolean enabled = true;
	/**
	 * Comma-separated list of data source names which should be excluded from monitoring.
	 */
	private String excludedDatasources;
	/**
	 * If /monitoring should be enabled for reports in the management http port instead of on the application http port.
	 */
	private boolean managementEndpointMonitoringEnabled;
	/**
	 * Map of initialization parameters to be passed to the JavaMelody monitoring filter.
	 * The available parameters are listed here: https://github.com/javamelody/javamelody/wiki/UserGuide#6-optional-parameters
	 */
	private Map<String, String> initParameters = new HashMap<>();

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
	 * Returns a comma-separated list of data source names which should be excluded from monitoring.
	 *
	 * @return Data source names to exclude from monitoring.
	 */
	public String getExcludedDatasources() {
		return excludedDatasources;
	}

	/**
	 * Returns if /monitoring should be enabled for reports in the management http port instead of on the application http port.
	 * @return true or false
	 */
	public boolean isManagementEndpointMonitoringEnabled() {
		return managementEndpointMonitoringEnabled;
	}

	/**
	 * Sets whether /monitoring should be enabled for reports in the management http port instead of on the application http port.
	 * @param managementEndpointEnabled true or false
	 */
	public void setManagementEndpointMonitoringEnabled(boolean managementEndpointEnabled) {
		this.managementEndpointMonitoringEnabled = managementEndpointEnabled;
	}

	/**
	 * Sets a comma-separated list of data source names which should be excluded from monitoring.
	 *
	 * @param excludedDatasources Data source names to exclude from monitoring.
	 */
	public void setExcludedDatasources(String excludedDatasources) {
		this.excludedDatasources = excludedDatasources;
	}

	/**
	 * Returns a map of initialization parameters to be passed to the JavaMelody monitoring filter.
	 * The available parameters are listed here: https://github.com/javamelody/javamelody/wiki/UserGuide#6-optional-parameters
	 *
	 * @return Initialization parameters for the JavaMelody monitoring filter.
	 */
	public Map<String, String> getInitParameters() {
		return initParameters;
	}

	/**
	 * Sets a map of initialization parameters to be passed to the JavaMelody monitoring filter.
	 * The available parameters are listed here: https://github.com/javamelody/javamelody/wiki/UserGuide#6-optional-parameters
	 *
	 * @param initParameters Initialization parameters for the JavaMelody monitoring filter.
	 */
	public void setInitParameters(Map<String, String> initParameters) {
		this.initParameters = initParameters;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return "JavaMelodyConfigurationProperties [enabled=" + enabled + ", excludedDatasources="
				+ excludedDatasources + ", initParameters=" + initParameters + "]";
	}
}
