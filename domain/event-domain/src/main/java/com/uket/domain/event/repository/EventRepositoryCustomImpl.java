package com.uket.domain.event.repository;

import static com.uket.domain.event.entity.QEvents.events;

import com.querydsl.core.types.Projections;
import com.querydsl.jpa.impl.JPAQueryFactory;
import com.uket.domain.university.dto.UniversityDto;
import com.uket.domain.university.entity.QUniversity;
import java.time.LocalDate;
import java.util.List;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class EventRepositoryCustomImpl implements EventRepositoryCustom {

    private final JPAQueryFactory queryFactory;


    @Override
    public List<UniversityDto> searchUniversitiesByDate(LocalDate date) {

        QUniversity university = events.university;

        return queryFactory
                .select(Projections.constructor(UniversityDto.class,
                        university.id,
                        university.name,
                        university.logoUrl
                ))
                .from(events)
                .where(events.startDate.loe(date),
                        events.endDate.goe(date),
                        university.currentEvent.eq(events.id))
                .fetch();
    }
}
