package de.swa.easyvalidation;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.MethodDescriptor;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import de.swa.easyvalidation.constraints.ConstraintRef;
import de.swa.easyvalidation.groups.AndGroup;
import de.swa.easyvalidation.groups.ConstraintRefGroup;
import de.swa.easyvalidation.groups.ConstraintRefGroups;
import de.swa.easyvalidation.groups.OrGroup;
import de.swa.easyvalidation.util.IndexedPropertyHelper;
import de.swa.easyvalidation.util.IndexedPropertyHelper.IndexInfo;
import de.swa.easyvalidation.groups.ConstraintRefGroups.Logical;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class EasyValidator {

    private static Logger log = LoggerFactory.getLogger(EasyValidator.class);
    
    private static String defaultMandatoryMessage = "error.validation.property.mandatory";
    private static String defaultImmutableMessage = "error.validation.property.immutable";
    private static String defaultContentMessage = "error.validation.property.content";

    private static Map<PropertyDescriptor, GetterInfo> propertyToGetterReturnTypeCache = new HashMap<>();

    /**
     * Validates that the property exists for that class.
     * 
     * @param property
     * @param clazz
     * @return
     */
    public static Class<?> validateProperty(String property, Class<?> clazz) {
        GetterInfo cachedHit = propertyToGetterReturnTypeCache.get(new PropertyDescriptor(property, clazz));
        // log.debug("cachedHit for " + property + " " + cachedHit);
        return cachedHit != null ? cachedHit.getReturnType() : validatePropertyAndCache(property, clazz);
    }

    /**
     * E.g. "location.address.city" for class Article<br/>
     * 1st check: Article.location exist, cache ("location", Article.class) -> (getLocation(), Location.class)<br/>
     * 2nd check: Location.address exist, cache ("location.address", Article.class) -> (getAddress(), Address.class)
     * <br/>
     * 3rd check: Address.city exist, cache ("location.address.city", Article.class) -> (getCity(), String.class)
     */
    private static Class<?> validatePropertyAndCache(String nestedProperty, Class<?> clazz) {
        // Split a nested property into its parts; e.g. ["location", "address", "city"]
        String[] propertyParts = nestedProperty.split("\\.");
        Class<?> propertyClass = clazz;
        String propertyKey = "";
        GetterInfo getterReturnType = null; // TODO not useful?! Cache Method instead ...
        for (String propertyPart : propertyParts) {
            if (IndexedPropertyHelper.getIndexInfo(propertyPart) != null) {
                String propertyName = propertyPart.substring(0, propertyPart.indexOf('['));
                Method getterMethod = getGetterMethodOrFail(propertyName, propertyClass);
                if (List.class.isAssignableFrom(getterMethod.getReturnType())) {
                    // Process list (e.g. for property articles[*])
                    // 1. get list field (-> articles)
                    Field listField;
                    try {
                        listField = clazz.getDeclaredField(propertyName);
                    } catch (NoSuchFieldException | SecurityException e) {
                        throw new IllegalArgumentException(
                                "Property " + propertyName + " is not a declared field of " + clazz);
                    }
                    // 2. get generic type (-> Article.class)
                    ParameterizedType listType = (ParameterizedType) listField.getGenericType();
                    Class<?> listClass = (Class<?>) listType.getActualTypeArguments()[0];
                    // 3. get getter method (-> getArticles())
                    getterReturnType = getGetterReturnType(getterMethod);
                    // 4. ignore return type 'java.util.List' and remember the type of its get(int) call (-> Article)
                    getterReturnType.setReturnType(listClass); // is this to tricky?
                    propertyClass = listClass;
                } else if (getterMethod.getReturnType().isArray()) {
                    // process array
                    Class<?> arrayTypeClass = getterMethod.getReturnType().getComponentType();
                    getterReturnType = getGetterReturnType(getterMethod);
                    getterReturnType.setReturnType(arrayTypeClass); // is this to tricky?
                    propertyClass = arrayTypeClass;
                } else {
                    // Exception? ...
                }
            } else {
                // process 'simple' property
                Method getterMethod = getGetterMethodOrFail(propertyPart, propertyClass);
                getterReturnType = getGetterReturnType(getterMethod);
                propertyClass = getterReturnType.getReturnType();
            }
            propertyKey += "." + propertyPart;
            propertyToGetterReturnTypeCache.put(new PropertyDescriptor(propertyKey.substring(1), clazz),
                    getterReturnType);
        }
    
        return propertyClass;
    }

    
    public static List<String> validateMandatoryConditions(Object object,
            ValidationConditions<?> validationConditions) {
        log.debug("Checking mandatory conditions for " + validationConditions.getTypeClass().getName());
        // Implementation note: keySet has same order as the LinkedHashMap!
        return validateMandatoryConditions(object, validationConditions.getMandatory().keySet(), validationConditions);
    }

    public static List<String> validateMandatoryConditions(Object object, Set<String> properties,
            ValidationConditions<?> validationConditions) {

        Map<String, ConstraintRefGroups> mandatoryConditions = validationConditions.getMandatory();

        validateArgumentsOrFail(object, properties, mandatoryConditions, validationConditions.getTypeClass());

        // TODO better returnType ...
        List<String> errors = new ArrayList<>();
        for (String property : properties) {
            ConstraintRefGroups groups = mandatoryConditions.get(property);
            // Check if all conditions are met. If so, the property value must not be null.
            log.debug(">>> Checking mandatory conditions for property '" + property + "'");
            if (groupsConditionsAreMet(groups, object)) {
                Object value = getPropertyResultObject(property, object);
                log.debug("Property '" + property + "' IS mandatory because all conditions are met.");
                if (value == null) {
                    errors.add(property + " - " + defaultMandatoryMessage);
                }
            } else {
                log.debug("Property '" + property + "' is NOT mandatory because some conditions are not met");
            }
        }
        return errors;
    }

    public static List<String> validateImmutableConditions(Object originalObject, Object modifiedObject,
            ValidationConditions<?> validationConditions) {
        return validateImmutableConditions(originalObject, modifiedObject, validationConditions);
    }

    public static List<String> validateImmutableConditions(Object originalObject, Object modifiedObject,
            Set<String> properties, ValidationConditions<?> validationConditions) {

        Map<String, ConstraintRefGroups> immutableConditions = validationConditions.getImmutable();
        Class<?> validationTypeClass = validationConditions.getTypeClass();

        validateArgumentsOrFail(originalObject, properties, immutableConditions, validationTypeClass);
        validateArgumentsOrFail(modifiedObject, properties, immutableConditions, validationTypeClass);

        // TODO better returnType ...
        List<String> errors = new ArrayList<>();
        for (String property : properties) {
            ConstraintRefGroups groups = immutableConditions.get(property);
            // Check if all conditions are met. If so, both property values must be equal
            log.debug(">>> Checking immutable conditions for property '" + property + "'");
            if (groupsConditionsAreMet(groups, originalObject)) {
                log.debug("Property '" + property + "' IS immutable because all conditions are met");
                Object originalValue = getPropertyResultObject(property, originalObject);
                Object modifiedValue = getPropertyResultObject(property, modifiedObject);
                if (!Objects.equals(originalValue, modifiedValue)) {
                    errors.add(property + " - " + defaultImmutableMessage);
                }
            } else {
                log.debug("Property '" + property + "' is NOT immutable because some conditions are not met.");
            }
        }
        return errors;
    }

    // Does some error checking
    private static void validateArgumentsOrFail(Object object, Set<String> properties,
            Map<String, ConstraintRefGroups> mandatoryConditions, Class<?> typeClass) {
        if (object == null || mandatoryConditions == null || mandatoryConditions.isEmpty()) {
            throw new IllegalArgumentException("Arguments must not be null resp. empty");
        }
        if (!object.getClass().equals(typeClass)) {
            throw new IllegalArgumentException("The object type (" + object.getClass()
                    + " does not equal the type of the ValidationConditionsMap (" + typeClass + ")");
        }
        for (String property : properties) {
            ConstraintRefGroups groups = mandatoryConditions.get(property);
            if (groups == null) {
                throw new IllegalArgumentException("No condition exist for property " + property);
            }
        }
    }

    public static Object getPropertyResultObject(String property, Object object) {
        String[] propertyParts = property.split("\\.");
        String propertyKey = "";
        Object propertyObject = object;
        Object returnValue = null;
        for (String propertyPart : propertyParts) {
            propertyKey += "." + propertyPart;
            PropertyDescriptor cacheKey = new PropertyDescriptor(propertyKey.substring(1), object.getClass());
            GetterInfo getter = propertyToGetterReturnTypeCache.get(cacheKey);
            try {
                returnValue = getter.getMethod().invoke(propertyObject);
                // If e.g. for "foo.bar" getFoo() returns null, we should prevent a NPE
                // TODO or is it better to throw IllArgEx? Or make this configurable?
                if (returnValue == null) {
                    break;
                }
                IndexInfo indexInfo = IndexedPropertyHelper.getIndexInfo(propertyKey.substring(1));
                // How to handle [*] etc.?
                /* propertyObject -> List<Object>, bei Simple-Props und Single-Ixd-Props ist size == 1
                 * Sonst loopen. Wenn IndexInfo != LIST or size > 1: propertyObject = new ArrayList<Object>
                 * propertyObject.addAll(returnValues) ...
                 */
                if (indexInfo != null) {
                    if (indexInfo.getIndexType() == IndexedPropertyHelper.IndexType.INCREMENT) {
                        throw new IllegalArgumentException("IndexType.INCREMENT not yet implemented");
                    }
                    if (indexInfo.getValues().size() != 1) {
                        log.debug("TODO - NOT SUPPORTED YET! -> 1st index is taken ...");
                    }
                    Integer index = indexInfo.getValues().get(0);
                    if (List.class.isAssignableFrom(returnValue.getClass())) {
                        // Process list
                        if (((List<?>) returnValue).size() > index) {
                            returnValue = ((List<?>) returnValue).get(index);
                            log.debug("list.get(" + index + "): " + returnValue);
                        } else {
                            log.warn("{} does not exist! Returning null. Or better throe an exception? ...", propertyPart);
                            returnValue = null;
                        }
                    } else if (returnValue.getClass().isArray()) {
                        // process array
                        if (Array.getLength(returnValue) > index) {
                            returnValue = Array.get(returnValue, index);
                            log.debug("array[" + index + "]: " + returnValue);
                        } else {
                            log.warn("{} does not exist! Returning null. Or better throe an exception? ...", propertyPart);
                            returnValue = null;
                        }
                    }
                }
            } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
                throw new RuntimeException(
                        "Exception while invoking method + " + getter.getMethod() + " on " + propertyObject, e);
            }
            if (returnValue == null) {
                return null;
            }
            propertyObject = returnValue;
        }
        return returnValue;
    }

    // If groups are ANDed each group must be met, if they are ORed only one must be met.
    private static boolean groupsConditionsAreMet(ConstraintRefGroups groups, Object object) {
        if (groups.getConstraintRefGroups().length == 0) {
            log.debug("Property is mandatory/immutable (no constraints)");
            return true;
        }
        Logical groupsOperator = groups.getLogicalOperator();
        for (ConstraintRefGroup group : groups.getConstraintRefGroups()) {
            if (groupIsMet(group, object)) {
                if (groupsOperator == Logical.OR) {
                    return true;
                }
            } else {
                if (groupsOperator == Logical.AND) {
                    return false;
                }
            }
        }
        return (groupsOperator == Logical.AND) ? true : false;
    }

    // All conditions of an AndGroup must be true, but only one of an OrGroup!
    private static boolean groupIsMet(ConstraintRefGroup group, Object object) {
        if (group instanceof AndGroup) {
            for (ConstraintRef contraint : ((AndGroup) group).getConstraintRefs()) {
                if (!contraintIsMet(contraint, object)) {
                    return false;
                }
            }
            return true;
        } else if (group instanceof OrGroup) {
            for (ConstraintRef contraint : ((OrGroup) group).getConstraintRefs()) {
                if (contraintIsMet(contraint, object)) {
                    return true;
                }
            }
            return false;
        } else {
            throw new IllegalArgumentException("Wrong ConditionGroup type ...");
        }
    }

    // TODO? handle array resp. list of values for e.g. articled[*].name ...
    private static boolean contraintIsMet(ConstraintRef contraintRef, Object object) {
        Object value = getPropertyResultObject(contraintRef.getProperty(), object);
        log.debug("Value of property '" + contraintRef.getProperty() + "' is '" + value + "'");
        if (value == null) {
            return false;
        }
        return contraintRef.getConstraint().validate(value, object);
    }
    
    private static Method getGetterMethodOrFail(String propertyName, Class<?> clazz) {
        Map<String, Method> noArgGetters = getNoArgGetterMethodMap(clazz);
        Method getterMethod = noArgGetters.get(buildGetterName(propertyName));
        if (getterMethod == null) {
            throw new IllegalArgumentException(
                    "No no-arg getter found for property " + propertyName + " in " + clazz.getName());
        }
        return getterMethod;
    }

    private static GetterInfo getGetterReturnType(Method getterMethod) {
        return new GetterInfo(getterMethod, getterMethod.getReturnType());
    }

    private static Map<String, Method> getNoArgGetterMethodMap(Class<?> clazz) {
        BeanInfo beanInfo;
        try {
            beanInfo = Introspector.getBeanInfo(clazz, Object.class);
        } catch (IntrospectionException e) {
            throw new RuntimeException("Introspection of bean info for " + clazz.getName() + " failed:", e);
        }
        return getNoArgGetterMethodMap(beanInfo);
    }

    private static Map<String, Method> getNoArgGetterMethodMap(BeanInfo beanInfo) {
        Map<String, Method> methodNamesMap = new HashMap<>();
        for (MethodDescriptor md : beanInfo.getMethodDescriptors()) {
            // log.debug("MethodDescriptor: " + md);
            if (md.getName().startsWith("get") && md.getMethod().getParameterTypes().length == 0) {
                methodNamesMap.put(md.getName(), md.getMethod());
                // log.debug("No-arg getter found: " + md);
            }
        }
        return methodNamesMap;
    }

    private static String buildGetterName(String propertyName) {
        return "get" + propertyName.substring(0, 1).toUpperCase() + propertyName.substring(1);
    }

    
    public static void setDefaultMandatoryMessage(String message) {
        defaultMandatoryMessage = message;
    }

    public static String getDefaultMandatoryMessage() {
        return defaultMandatoryMessage;
    }

    public static void setDefaultImmutableMessage(String message) {
        defaultImmutableMessage = message;
    }

    public static String getDefaultImmutableMessage() {
        return defaultImmutableMessage;
    }

    public static void setDefaultContentMessage(String message) {
        defaultContentMessage = message;
    }

    public static String getDefaultContentMessage() {
        return defaultContentMessage;
    }


    private static class PropertyDescriptor {
        private String propertyName;
        private Class<?> clazz;

        public PropertyDescriptor(String propertyName, Class<?> clazz) {
            super();
            this.propertyName = propertyName;
            this.clazz = clazz;
        }

        @Override
        public String toString() {
            return "PropertyDescriptor [propertyName=" + propertyName + ", clazz=" + clazz + "]";
        }

        // java.lang.Class does not implement hashCode() and equals(), but identity hash code is o.k. here, or?
        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + ((clazz == null) ? 0 : clazz.hashCode());
            result = prime * result + ((propertyName == null) ? 0 : propertyName.hashCode());
            return result;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            PropertyDescriptor other = (PropertyDescriptor) obj;
            if (clazz == null) {
                if (other.clazz != null)
                    return false;
            } else if (!clazz.equals(other.clazz))
                return false;
            if (propertyName == null) {
                if (other.propertyName != null)
                    return false;
            } else if (!propertyName.equals(other.propertyName))
                return false;
            return true;
        }
    }

    
    static class GetterInfo {
        private Method method;
        private Class<?> returnType;

        public GetterInfo(Method method, Class<?> returnType) {
            super();
            this.method = method;
            this.returnType = returnType;
        }

        public Method getMethod() {
            return method;
        }

        public Class<?> getReturnType() {
            return returnType;
        }

        public void setReturnType(Class<?> returnType) {
            this.returnType = returnType;
        }

        @Override
        public String toString() {
            return "GetterInfo [method=" + method + ", returnType=" + returnType + "]";
        }
    }
}
