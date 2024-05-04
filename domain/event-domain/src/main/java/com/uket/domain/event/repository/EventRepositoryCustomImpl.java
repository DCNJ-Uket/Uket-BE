package com.uket.domain.event.repository;

import static com.uket.domain.event.entity.QEvents.events;

import com.querydsl.core.types.Projections;
import com.querydsl.jpa.impl.JPAQueryFactory;
import com.uket.domain.university.dto.UniversityDto;
import java.time.LocalDate;
import java.util.List;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class EventRepositoryCustomImpl implements EventRepositoryCustom {

    private final JPAQueryFactory queryFactory;


    @Override
    public List<UniversityDto> searchUniversitiesByDate(LocalDate date) {

        return queryFactory
                .select(Projections.constructor(UniversityDto.class,
                        events.university.id,
                        events.university.name,
                        events.university.logoUrl
                ))
                .from(events)
                .where(events.startDate.loe(date),
                        events.endDate.goe(date),
                        events.university.currentEvent.eq(events.id))
                .fetch();
    }
}
