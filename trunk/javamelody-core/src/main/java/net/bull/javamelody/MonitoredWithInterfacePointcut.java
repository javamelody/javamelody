package net.bull.javamelody;

import org.springframework.aop.ClassFilter;
import org.springframework.aop.MethodMatcher;
import org.springframework.aop.Pointcut;

/**
 * Pointcut that identifies methods/classes with the {@link MonitoredWithSpring} annotation.
 *
 * @author Erik van Oosten (Java Simon, Licence LGPL)
 */
public class MonitoredWithInterfacePointcut implements Pointcut {
	Class<?> interfaceClass;

	private final ClassFilter classFilter = new ClassFilter() {
		/** {@inheritDoc} */
		@SuppressWarnings("unchecked")
		public boolean matches(Class clazz) {
			return interfaceClass.isAssignableFrom(clazz);
		}
	};

	/**
	 * @return a class filter based on interfaceName.
	 */
	public ClassFilter getClassFilter() {
		return classFilter;
	}

	/**
	 * @return a method matcher that matches any method
	 */
	public MethodMatcher getMethodMatcher() {
		return MethodMatcher.TRUE;
	}

	/**
	 * Retourne le nom de l'interface à matcher.
	 * @return String
	 */
	public String getInterfaceName() {
		if (interfaceClass != null) {
			return interfaceClass.getName();
		}
		return null;
	}

	/**
	 * Définit le nom de l'interface à matcher (non null).
	 * @param interfaceName String
	 * @throws ClassNotFoundException Si classe non trouvée
	 */
	public void setInterfaceName(String interfaceName) throws ClassNotFoundException {
		assert interfaceName != null;
		this.interfaceClass = Class.forName(interfaceName);
	}
}
