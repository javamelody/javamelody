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

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.quartz.autoconfigure.SchedulerFactoryBeanCustomizer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Role;

/**
 * Spring Boot auto-configuration to customize Quartz for JavaMelody.
 *
 * @author Emeric Vernat
 */
@Configuration
@ConditionalOnBean(JavaMelodyAutoConfiguration.class)
@ConditionalOnClass(name = {
		"org.springframework.boot.quartz.autoconfigure.SchedulerFactoryBeanCustomizer",
		"org.springframework.scheduling.quartz.SchedulerFactoryBean",
		"org.quartz.JobListener" })
@Role(BeanDefinition.ROLE_INFRASTRUCTURE)
public class JavaMelodyQuartzAutoConfiguration {
	/**
	 * Configure Spring's Schedulers for Quartz Scheduler
	 * @return SchedulerFactoryBeanCustomizer
	 */
	@Bean
	@ConditionalOnMissingBean
	public SchedulerFactoryBeanCustomizer schedulerFactoryBeanCustomizer() {
		return schedulerFactoryBean -> {
			final JobGlobalListener jobGlobalListener = new JobGlobalListener();
			schedulerFactoryBean.setGlobalJobListeners(jobGlobalListener);
			schedulerFactoryBean.setExposeSchedulerInRepository(true);
		};
	}
}
