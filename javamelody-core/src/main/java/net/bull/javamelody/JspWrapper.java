/*
 * Copyright 2008-2010 by Emeric Vernat
 *
 *     This file is part of Java Melody.
 *
 * Java Melody is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Java Melody is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Java Melody.  If not, see <http://www.gnu.org/licenses/>.
 */
package net.bull.javamelody;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.servlet.RequestDispatcher;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;

/**
 * Wrapping de l'interface javax.servlet.RequestDispatcher pour avoir les temps moyens de rendu
 * des pages JSP.
 * @author Emeric Vernat
 */
final class JspWrapper implements InvocationHandler {
	private static final Counter JSP_COUNTER = new Counter(Counter.JSP_COUNTER_NAME, "jsp.png",
			JdbcWrapper.SINGLETON.getSqlCounter());
	private static final boolean COUNTER_HIDDEN = Parameters.isCounterHidden(JSP_COUNTER.getName());
	private static final boolean DISABLED = Boolean.parseBoolean(Parameters
			.getParameter(Parameter.DISABLED));

	private final String path;
	private final RequestDispatcher requestDispatcher;

	private static class HttpRequestWrapper extends HttpServletRequestWrapper {
		/**
		 * Constructs a request object wrapping the given request.
		 * @param request HttpServletRequest
		 */
		HttpRequestWrapper(HttpServletRequest request) {
			super(request);
		}

		/** {@inheritDoc} */
		@Override
		public RequestDispatcher getRequestDispatcher(String path) {
			final RequestDispatcher requestDispatcher = super.getRequestDispatcher(path);
			if (requestDispatcher == null) {
				return null;
			}
			// il n'est pas dit que path soit non null
			final InvocationHandler invocationHandler = new JspWrapper(String.valueOf(path),
					requestDispatcher);
			return JdbcWrapper.createProxy(requestDispatcher, invocationHandler);
		}
	}

	/**
	 * Constructeur.
	 * @param path String
	 * @param requestDispatcher RequestDispatcher
	 */
	JspWrapper(String path, RequestDispatcher requestDispatcher) {
		super();
		assert path != null;
		assert requestDispatcher != null;
		// quand ce RequestDispatcher est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		JSP_COUNTER.setDisplayed(!COUNTER_HIDDEN);
		JSP_COUNTER.setUsed(true);
		this.path = path;
		this.requestDispatcher = requestDispatcher;
	}

	static HttpServletRequest createHttpRequestWrapper(HttpServletRequest request) {
		if (DISABLED || COUNTER_HIDDEN) {
			return request;
		}
		return new HttpRequestWrapper(request);
	}

	static Counter getJspCounter() {
		return JSP_COUNTER;
	}

	/**
	 * Intercepte une exécution de méthode sur une façade.
	 * @param proxy Object
	 * @param method Method
	 * @param args Object[]
	 * @return Object
	 * @throws Throwable t
	 */
	public Object invoke(Object proxy, Method method, Object[] args) throws Throwable {
		final String methodName = method.getName();
		if (!"include".equals(methodName) && !"forward".equals(methodName)) {
			return method.invoke(requestDispatcher, args);
		}
		boolean systemError = false;
		try {
			final String pathWithoutParameters;
			if (path.indexOf('?') != -1) {
				pathWithoutParameters = path.substring(0, path.indexOf('?'));
			} else {
				pathWithoutParameters = path;
			}
			JSP_COUNTER.bindContextIncludingCpu(pathWithoutParameters);
			return method.invoke(requestDispatcher, args);
		} catch (final InvocationTargetException e) {
			if (e.getCause() instanceof Error) {
				// on catche Error pour avoir les erreurs systèmes
				// mais pas Exception qui sont fonctionnelles en général
				systemError = true;
			}
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			JSP_COUNTER.addRequestForCurrentContext(systemError);
		}
	}
}
