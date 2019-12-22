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

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to mark classes and/or methods that should be monitored by {@link MonitoringSpringInterceptor}.
 * <p/>
 * A method is monitored when it is annotated, or when it is in a class that is annotated.
 * <p/>
 * The monitor name consists of 2 parts, concatenated with a dot ".":
 * -1- the class name
 * -2- the method name
 * <p/>
 * If the name attribute is specified on a class it will override the class name part.
 * Default: the simple class name.
 * <p/>
 * If the name attribute is specified on a method it will override the method name part.
 * Default: the name of the method.
 *
 * Inspired by Erik van Oosten (Java Simon, Licence LGPL)
 * @author Emeric Vernat
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.METHOD, ElementType.TYPE })
public @interface MonitoredWithSpring {
	/**
	 * @see MonitoredWithSpring
	 * @return String
	 */
	String name() default "";
}
