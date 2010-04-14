package net.bull.javamelody;

import java.io.Serializable;

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.springframework.aop.support.AopUtils;

/**
 * Method interceptor that measures the duration of the intercepted call.
 *
 * @author Erik van Oosten (Java Simon, Licence LGPL)
 */
public class MonitoringSpringInterceptor implements MethodInterceptor, Serializable {
	private static final long serialVersionUID = -6594338383847482623L;
	private static final Counter SPRING_COUNTER = MonitoringProxy.getSpringCounter();
	private static final boolean COUNTER_HIDDEN = Parameters.isCounterHidden(SPRING_COUNTER
			.getName());
	private static final boolean DISABLED = Boolean.parseBoolean(Parameters
			.getParameter(Parameter.DISABLED));

	/**
	 * Constructeur.
	 */
	public MonitoringSpringInterceptor() {
		super();
		// quand cet intercepteur est utilisé, le compteur est affiché
		// sauf si le paramètre displayed-counters dit le contraire
		SPRING_COUNTER.setDisplayed(!COUNTER_HIDDEN);
	}

	/**
	 * Performs method invocation.
	 * Auteur: Emeric Vernat
	 *
	 * @param invocation method invocation
	 * @return return object from the method
	 * @throws Throwable anything thrown by the method
	 */
	public Object invoke(MethodInvocation invocation) throws Throwable {
		// cette méthode est appelée par spring aop
		if (DISABLED || !SPRING_COUNTER.isDisplayed()) {
			return invocation.proceed();
		}
		// nom identifiant la requête
		final String requestName = getMonitorName(invocation);

		boolean systemError = false;
		try {
			SPRING_COUNTER.bindContextIncludingCpu(requestName);
			return invocation.proceed();
		} catch (final Error e) {
			// on catche Error pour avoir les erreurs systèmes
			// mais pas Exception qui sont fonctionnelles en général
			systemError = true;
			throw e;
		} finally {
			// on enregistre la requête dans les statistiques
			SPRING_COUNTER.addRequestForCurrentContext(systemError);
		}
	}

	/**
	 * Determine monitor name for a method invocation.
	 *
	 * @param invocation the method invocation (not null)
	 * @return the monitor name for this invocation
	 */
	private static String getMonitorName(MethodInvocation invocation) {
		final String classPart = getClassPart(invocation);
		final String methodPart = getMethodPart(invocation);
		return classPart + '.' + methodPart;
	}

	@SuppressWarnings("unchecked")
	private static String getClassPart(MethodInvocation invocation) {
		final Class targetClass = AopUtils.getTargetClass(invocation.getThis());
		final MonitoredWithSpring classAnnotation = (MonitoredWithSpring) targetClass
				.getAnnotation(MonitoredWithSpring.class);
		if (classAnnotation == null || classAnnotation.name() == null
				|| classAnnotation.name().length() == 0) {
			return targetClass.getSimpleName();
		}
		return classAnnotation.name();
	}

	private static String getMethodPart(MethodInvocation invocation) {
		final MonitoredWithSpring methodAnnotation = invocation.getMethod().getAnnotation(
				MonitoredWithSpring.class);
		if (methodAnnotation == null || methodAnnotation.name() == null
				|| methodAnnotation.name().length() == 0) {
			return invocation.getMethod().getName();
		}
		return methodAnnotation.name();
	}
}
