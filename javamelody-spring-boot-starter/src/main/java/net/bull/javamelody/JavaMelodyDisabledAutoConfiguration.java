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

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Spring Boot auto-configuration for disabled JavaMelody.
 *
 * This class is picked up by the Spring Boot auto-configuration mechanism.
 * It disables JavaMelody initialization by the servlet container, when the Spring's property javamelody.enabled is false,
 * for the case when the webapp is deployed as war in a servlet container.
 */
@Configuration
@ConditionalOnProperty(prefix = JavaMelodyConfigurationProperties.PREFIX, name = "enabled", havingValue = "false", matchIfMissing = false)
public class JavaMelodyDisabledAutoConfiguration {
	/**
	 * Executed when Spring has property javamelody.enabled=false
	 * @return "true"
	 */
	@Bean
	public String javamelodyDisabled() {
		Parameter.DISABLED.setValue("true");
		return "true";
	}
}
