/*
 * Copyright © 2016 DV Bern AG, Switzerland
 *
 * Das vorliegende Dokument, einschliesslich aller seiner Teile, ist urheberrechtlich
 * geschützt. Jede Verwertung ist ohne Zustimmung der DV Bern AG unzulässig. Dies gilt
 * insbesondere für Vervielfältigungen, die Einspeicherung und Verarbeitung in
 * elektronischer Form. Wird das Dokument einem Kunden im Rahmen der Projektarbeit zur
 * Ansicht übergeben, ist jede weitere Verteilung durch den Kunden an Dritte untersagt.
 */

package net.bull.javamelody;
import java.lang.reflect.Method;

import javax.persistence.EntityManager;

public interface JpaNamingStrategy {

	/**
	 * Implementors must calculate a nonnull String that will get displayed in the JPA section.
	 *
	 * The implementing class <b>must</b> habe a public no-args constructor.
	 *
	 * @param jpaMethod A normalization of the method that got called on the {@link EntityManager}.
	 * 					Corresponds with param javaMethod.
	 * @param javaMethod The method that got called on the {@link EntityManager}.
	 * 					 Corresponds with param jpaMethod.
	 * @param args Nullable, the arguments for javaMethod
	 * @return a non-null String that represents the request name of the JPA-Counter.
	 */
	String getRequestName(JpaMethod jpaMethod, Method javaMethod, Object[] args);
}
