package de.swa.easyvalidation;

import org.junit.Test;

import java.math.BigInteger;
import java.time.LocalDate;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import static org.junit.Assert.*;

public class EasyValidatorTest {

    final Reservation reservation1 = new Reservation(101, ReservationStatus.NEW, new Customer("Donald Duck"),
            Arrays.asList(new Article("Endoscope")));

    // besser Article.location.city ?

    @Test
    public void validateProperty_simpleProperty() {
        try {
            EasyValidator.validateProperty("status", Reservation.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public void validateProperty_simplePropertyInherited() {
        try {
            EasyValidator.validateProperty("id", Reservation.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test
    public void validateProperty_simplePropertyNested() {
        try {
            EasyValidator.validateProperty("firstArticle.name", Reservation.class);
        } catch (IllegalArgumentException e) {
            fail();
        }
    }

    @Test(expected = IllegalArgumentException.class)
    public void validateProperty_notexisting() {
        EasyValidator.validateProperty("notexisting", Reservation.class);
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