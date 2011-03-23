package net.bull.javamelody;

import javax.sql.DataSource;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.config.AbstractFactoryBean;

/**
 * Spring {@link FactoryBean} for wrapping datasources with monitoring proxy.
 *
 * @see AbstractFactoryBean
 * @see JdbcWrapper
 *
 * @author et2448, Emeric Vernat
 */
public class SpringDataSourceFactoryBean extends AbstractFactoryBean {
	private String targetName;

	// exemple :
	//	<bean id="wrappedDataSource" class="net.bull.javamelody.SpringDataSourceFactoryBean">
	//		<property name="targetName" value="targetDataSource" />
	//	</bean>
	//	
	//	<bean id="targetDataSource" ...
	//	</bean>

	/**
	 * Name of target bean to instrument.
	 * This should implement {@linkplain DataSource}.
	 * @param targetName name of bean, not null.
	 */
	public void setTargetName(String targetName) {
		assert targetName != null;
		this.targetName = targetName;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected DataSource createInstance() {
		if (targetName == null) {
			throw new IllegalStateException("targetName must not be null");
		}
		final DataSource dataSource = (DataSource) getBeanFactory().getBean(targetName,
				DataSource.class);
		JdbcWrapper.registerSpringDataSource(targetName, dataSource);
		final DataSource result = JdbcWrapper.SINGLETON.createDataSourceProxy(targetName,
				dataSource);
		LOG.debug("Spring target datasource wrapped: " + targetName);
		return result;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Class<?> getObjectType() {
		return DataSource.class;
	}
}
