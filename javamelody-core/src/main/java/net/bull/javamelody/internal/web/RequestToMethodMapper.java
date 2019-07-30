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
package net.bull.javamelody.internal.web;

import java.io.IOException;
import java.lang.annotation.Annotation;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import net.bull.javamelody.internal.common.HttpParameter;
import net.bull.javamelody.internal.common.HttpPart;

/**
 * Maps of http requests to methods of controllers, based on request parameters and annotations on methods and parameters.
 * @param <T> Type of the controller having methods annotated with @HttpPart
 * @author Emeric Vernat
 */
class RequestToMethodMapper<T> {
	private final Map<HttpPart, Method> methodsByPart = new HashMap<HttpPart, Method>();

	@Target(ElementType.METHOD)
	@Retention(RetentionPolicy.RUNTIME)
	@interface RequestPart {
		/**
		 * @return String
		 */
		HttpPart value();
	}

	@Target(ElementType.PARAMETER)
	@Retention(RetentionPolicy.RUNTIME)
	@interface RequestParameter {
		/**
		 * @return String
		 */
		HttpParameter value();
	}

	@Target(ElementType.PARAMETER)
	@Retention(RetentionPolicy.RUNTIME)
	@interface RequestAttribute {
		/**
		 * @return String
		 */
		String value();
	}

	@Target(ElementType.PARAMETER)
	@Retention(RetentionPolicy.RUNTIME)
	@interface RequestHeader {
		/**
		 * @return String
		 */
		String value();
	}

	RequestToMethodMapper(Class<T> clazz) {
		super();
		for (final Method method : clazz.getDeclaredMethods()) {
			final RequestPart partAnnotation = method.getAnnotation(RequestPart.class);
			if (partAnnotation != null) {
				methodsByPart.put(partAnnotation.value(), method);
			}
		}
	}

	void invoke(HttpServletRequest httpRequest, T controller) throws IOException {
		invokeAndReturn(httpRequest, controller);
	}

	Object invokeAndReturn(HttpServletRequest httpRequest, T controller) throws IOException {
		final String partParameter = HttpParameter.PART.getParameterFrom(httpRequest);
		final HttpPart httpPart = HttpPart.getByName(partParameter);
		final Method method = methodsByPart.get(httpPart);
		if (method == null) {
			throw new IllegalArgumentException("Unknown http part: " + partParameter);
		}
		try {
			// find parameters values
			final Object[] parameterValues = getParameterValues(httpRequest, method);
			// invoke the method (the "endpoint")
			return method.invoke(controller, parameterValues);
		} catch (final IllegalAccessException e) {
			throw new IllegalStateException(e);
		} catch (final InvocationTargetException e) {
			final Throwable targetException = e.getTargetException();
			if (targetException instanceof IOException) {
				throw (IOException) targetException;
			} else if (targetException instanceof RuntimeException) {
				throw (RuntimeException) targetException;
			} else if (targetException instanceof Error) {
				throw (Error) targetException;
			}
			throw new IOException(targetException);
		}
	}

	private Object[] getParameterValues(HttpServletRequest request, Method method) {
		// note: method.getParameters() existe seulement depuis Java 8
		final Annotation[][] parameters = method.getParameterAnnotations();
		final Object[] values = new Object[parameters.length];
		for (int i = 0; i < parameters.length; i++) {
			final Annotation[] parameter = parameters[i];
			boolean found = false;
			for (final Annotation annotation : parameter) {
				if (annotation.annotationType() == RequestParameter.class) {
					final HttpParameter requestParameter = ((RequestParameter) annotation).value();
					values[i] = requestParameter.getParameterFrom(request);
					found = true;
					break;
				} else if (annotation.annotationType() == RequestAttribute.class) {
					final String requestAttribute = ((RequestAttribute) annotation).value();
					values[i] = request.getAttribute(requestAttribute);
					found = true;
					break;
				} else if (annotation.annotationType() == RequestHeader.class) {
					final String requestHeader = ((RequestHeader) annotation).value();
					values[i] = request.getHeader(requestHeader);
					found = true;
					break;
				}
			}
			if (!found) {
				throw new IllegalStateException(
						"a parameter not annotated in method " + method.getName());
			}
		}
		return values;
	}
}
