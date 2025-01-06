package com.uket.domain.terms.repository;

import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import com.uket.domain.terms.entity.TermsSign;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface TermsSignRepository extends JpaRepository<TermsSign, Long> {

    @Query("""
        SELECT ts
        FROM TermsSign ts
        WHERE ts.userId = :userId
          AND ts.termsId IN :termsIds
          AND ts.agreedAt = (
              SELECT MAX(subTs.agreedAt)
              FROM TermsSign subTs
              WHERE subTs.termsId = ts.termsId AND subTs.userId = ts.userId
          )
    """)
    List<TermsSign> findLatestByUserIdAndTermsIds(
            @Param("userId") Long userId,
            @Param("termsIds") List<Long> termsIds
    );
}
