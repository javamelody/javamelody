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

import java.io.IOException;

import org.springframework.http.HttpRequest;
import org.springframework.http.client.ClientHttpRequestExecution;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.web.client.RestTemplate;

import net.bull.javamelody.internal.model.Counter;

/**
 * Interceptor for Spring {@link RestTemplate}.
 * @author Emeric Vernat
 */
public class SpringRestTemplateInterceptor implements ClientHttpRequestInterceptor {
	static final ClientHttpRequestInterceptor SINGLETON = new SpringRestTemplateInterceptor();

	private static final Counter SPRING_COUNTER = MonitoringProxy.getSpringCounter();

	/** {@inheritDoc} */
	@Override
	public ClientHttpResponse intercept(HttpRequest httpRequest, byte[] body,
			ClientHttpRequestExecution execution) throws IOException {
		if (!SPRING_COUNTER.isDisplayed()) {
			return execution.execute(httpRequest, body);
		}
		// nom identifiant la requête
		final String requestName = getRequestName(httpRequest);

		boolean systemError = false;
		try {
			SPRING_COUNTER.bindContextIncludingCpu(requestName);
			// we could add in httpRequest.getHeaders() a javamelody id
			// to link, in the collector server, the callers in this jvm to the callees in the jvm called
			return execution.execute(httpRequest, body);
		} catch (final IOException e) {
			// IOException - in case of I/O errors
			systemError = true;
			throw e;
		} catch (final Error e) {
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			SPRING_COUNTER.addRequestForCurrentContext(systemError);
		}
	}

	protected String getRequestName(HttpRequest httpRequest) {
		String uri = httpRequest.getURI().toString();
		final int index = uri.indexOf('?');
		if (index != -1) {
			uri = uri.substring(0, index);
		}
		return uri + ' ' + httpRequest.getMethod();
	}
}
