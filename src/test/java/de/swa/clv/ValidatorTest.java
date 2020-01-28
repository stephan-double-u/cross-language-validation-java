package de.swa.clv;

import de.swa.clv.constraints.Equals;
import org.junit.Ignore;
import org.junit.Test;

import java.math.BigInteger;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import static org.junit.Assert.*;

public class ValidatorTest {

    final Reservation reservation1 = new Reservation(101, ReservationStatus.NEW, new Customer("Donald Duck"),
            Arrays.asList(new Article("Endoscope")));

    // besser Article.location.city ?

    @Test
    public void validateProperty_simpleProperty() {
        try {
            Validator.validateProperty("status", Reservation.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public void validateProperty_simplePropertyInherited() {
        try {
            Validator.validateProperty("id", Reservation.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public void validateProperty_simplePropertyNested() {
        try {
            Validator.validateProperty("firstArticle.name", Reservation.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void validateProperty_notexisting() {
        Validator.validateProperty("notexisting", Reservation.class);
    }


    @Test
    @Ignore
    public void todoTestIndexProps() {
        ValidationRules<ClassUnderTest> rules = new ValidationRules<>(ClassUnderTest.class);
        //rules.content("stringArrayProp[0]", Equals.anyRef("stringArrayProp[0]"), Constraint.ref("stringArrayProp[0]", Equals.anyRef("stringArrayProp[0]")));
        rules.content("subClassArrayProp[*].stringArrayProp[0-3]", Equals.any("foo"));
        final List<String> errors = Validator.validateContentRules(new ClassUnderTest(), rules);
        System.out.println("errors: " + errors);
    }


    class ClassUnderTest {
        private SubClass subClassProp = new SubClass("a1", new String[] {"b1", "c1"}, Arrays.asList("d1", "e1", "f1"));
        private SubClass[] subClassArrayProp = {
                new SubClass("a2", new String[] {"b2", "c2", "d2"}, Arrays.asList("e2", "f2")),
                new SubClass("a3", new String[] {"b3", "c3"}, Arrays.asList("d3", "e3", "f3"))};

        public SubClass getSubClassProp() {
            return subClassProp;
        }

        public SubClass[] getSubClassArrayProp() {
            return subClassArrayProp;
        }
    }

    class SubClass {
        private String stringProp = "foo";

        public SubClass(String stringProp, String[] stringArrayProp, List<String> stringListProp) {
            this.stringProp = stringProp;
            this.stringArrayProp = stringArrayProp;
            this.stringListProp = stringListProp;
        }

        private String[] stringArrayProp;
        private List<String> stringListProp;

        public String getStringProp() {
            return stringProp;
        }

        public String[] getStringArrayProp() {
            return stringArrayProp;
        }

        public List<String> getStringListProp() {
            return stringListProp;
        }
    }


    public static class Reservation extends ReservationVO implements Identifiable<Integer> {
        private final ReservationStatus status;
        private final Customer customer;
        private final List<String> stringList;
        private final List<Article> articleList;
        private final Article[] articleArray;

        private final Article firstArticle;
        private final String someString = "Endoscope";
        private final int someInt = 123;
        private final BigInteger someBigInteger = new BigInteger("12345678901234567890123456789012345678901234567890");
        private final Boolean someBoolean = true; //TODO supporting isSomeBoolean() getter ?!
        private final Date startDate = new Date(new Date().getTime() - 1);
        private Calendar startCalDate = null;
        private final LocalDate startLocalDate = LocalDate.now().minusDays(10);
        public Reservation(final Integer id, final ReservationStatus status, final Customer customer, final List<Article> articles) {
            super(id);
            this.status = status;
            this.customer = customer;
            stringList = Arrays.asList("one", "two");
            articleList = articles;
            articleArray = articles.toArray(new Article[0]);
            firstArticle = articles.get(0);
            startCalDate = Calendar.getInstance();
            startCalDate.set(2999, 12, 31);
        }
        @Override
        public Integer getId() {
            return super.getId() * 2;
        }
        public ReservationStatus getStatus() {
            return status;
        }
        public Customer getCustomer() {
            return customer;
        }
        public List<Article> getArticleList() {
            return articleList;
        }
        public Article[] getArticleArray() {
            return articleArray;
        }
        public Article getFirstArticle() {
            return firstArticle;
        }
        public String getSomeString() {
            return someString;
        }
        public int getSomeInt() {
            return someInt;
        }
        public BigInteger getSomeBigInteger() {
            return someBigInteger;
        }
        public Boolean getSomeBoolean() {
            return someBoolean;
        }
        public Date getStartDate() {
            return startDate;
        }
        public LocalDate getStartLocalDate() {
            return startLocalDate;
        }
        public Calendar getStartCalDate() {
            return startCalDate;
        }

        public List<String> getStringList() {
            return stringList;
        }
    }
    public static class ReservationVO {
        private final Integer id;
        public ReservationVO(final Integer id) {
            super();
            this.id = id;
        }
        public Integer getId() {
            return id;
        }
    }
    public static interface Identifiable<T> {
        T getId();
    }
    public static class Customer {
        private final String name;
        public Customer(final String name) {
            this.name = name;
        }
        public String getName() {
            return name;
        }
        @Override
        public int hashCode() {
            return 31;
        }
        @Override
        public boolean equals(final Object obj) {
            return true;
        }

    }
    public static class Article {
        private final String name;
        public Article(final String name) {
            this.name = name;
        }
        public String getName() {
            return name;
        }
    }
    public static enum ReservationStatus {
        NEW, APPROVED, DELIVERED, RETURNED
    }
    public static enum ArticleStatus {
        COMMISSIONING, NEW, ACTIVE, INACTIVE, DECOMMISSIONED
    }

}