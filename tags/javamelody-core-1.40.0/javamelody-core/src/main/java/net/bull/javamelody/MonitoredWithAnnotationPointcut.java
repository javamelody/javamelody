package net.bull.javamelody;

import java.lang.reflect.Method;

import org.springframework.aop.ClassFilter;
import org.springframework.aop.MethodMatcher;
import org.springframework.aop.Pointcut;

/**
 * Pointcut that identifies methods/classes with the {@link MonitoredWithSpring} annotation.
 *
 * @author Erik van Oosten (Java Simon, Licence LGPL)
 */
public class MonitoredWithAnnotationPointcut implements Pointcut {
	/**
	 * @return a class filter that lets all class through.
	 */
	@Override
	public ClassFilter getClassFilter() {
		return ClassFilter.TRUE;
	}

	/**
	 * @return a method matcher that matches any method that has the {@link MonitoredWithSpring} annotation,
	 *         or is in a class with the {@link MonitoredWithSpring} annotation
	 */
	@Override
	public MethodMatcher getMethodMatcher() {
		return MonitoredMethodMatcher.INSTANCE;
	}

	private enum MonitoredMethodMatcher implements MethodMatcher {
		INSTANCE;

		/** {@inheritDoc} */
		@Override
		@SuppressWarnings({ "unchecked", "rawtypes" })
		public boolean matches(Method method, Class targetClass) {
			return targetClass.isAnnotationPresent(MonitoredWithSpring.class)
					|| method.getDeclaringClass().isAnnotationPresent(MonitoredWithSpring.class)
					|| method.isAnnotationPresent(MonitoredWithSpring.class);
		}

		/** {@inheritDoc} */
		@Override
		public boolean isRuntime() {
			return false;
		}

		/** {@inheritDoc} */
		@Override
		@SuppressWarnings("rawtypes")
		public boolean matches(Method method, Class targetClass, Object[] args) {
			throw new UnsupportedOperationException("This is not a runtime method matcher");
		}
	}
}
