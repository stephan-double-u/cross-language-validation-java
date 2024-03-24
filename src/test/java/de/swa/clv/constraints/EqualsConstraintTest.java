package de.swa.clv.constraints;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.jupiter.api.Assertions.*;

class EqualsConstraintTest {

    @Test
    void isSupportedType_trueForAnyNumericalPrimitive() {
        EqualsConstraint constraint = Equals.any(BigInteger.ONE);
        assertTrue(constraint.isSupportedType(byte.class));
        assertTrue(constraint.isSupportedType(short.class));
        assertTrue(constraint.isSupportedType(long.class));
        assertTrue(constraint.isSupportedType(int.class));
        assertTrue(constraint.isSupportedType(long.class));
        assertTrue(constraint.isSupportedType(float.class));
        assertTrue(constraint.isSupportedType(double.class));
    }

    @Test
    void isSupportedType_trueForAnyNumber() {
        EqualsConstraint constraint = Equals.any((short) 1);
        assertTrue(constraint.isSupportedType(Byte.class));
        assertTrue(constraint.isSupportedType(Short.class));
        assertTrue(constraint.isSupportedType(Long.class));
        assertTrue(constraint.isSupportedType(Integer.class));
        assertTrue(constraint.isSupportedType(Long.class));
        assertTrue(constraint.isSupportedType(Float.class));
        assertTrue(constraint.isSupportedType(Double.class));
    }


}
