package net.bull.javamelody;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation to mark classes and/or methods that should be monitored.
 * <p/>
 * A method is monitored when it is annotated, or when it is in a class that is annotated.
 * <p/>
 * The monitor name consists of 2 parts, concatenated with a dot ".":
 * -1- the class name
 * -2- the method name
 * <p/>
 * If the name attribute is specified on a class it will override the class name part.
 * Default: the full class name.
 * <p/>
 * If the name attribute is specified on a method it will override the method name part.
 * Default: the name of the method.
 *
 * @author Erik van Oosten (Java Simon, Licence LGPL)
 */
@Retention(value = RetentionPolicy.RUNTIME)
@Target({ ElementType.METHOD, ElementType.TYPE })
public @interface MonitoredWithGuice {
	/**
	 * @see MonitoredWithGuice
	 */
	String name() default "";
}
