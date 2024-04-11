# UKet

## 바로가기
> [API 명세서](http://localhost:8080/swagger-ui/index.html#/)  
> [카카오 로그인](http://localhost:8080/oauth2/authorization/kakao)  
> [구글 로그인](http://localhost:8080/oauth2/authorization/google)  

## 개발환경
| 구분 | 종류                             |
| --- |--------------------------------|
| Language | Java temurin 21.0.4            |
| Framework | Spring boot 3.2.4              |
| ORM | Spring Data JPA with Querydsl  |
| Build Tool | Gradle 8.4            |
| Database | h2 2.2.224, MySQL 8.2.0, redis |
| API Docs | SpringDoc Swagger 3            |
| Test | JUnit 5                        |

## 환경변수
| 변수                   | 설명                       |
|----------------------|--------------------------|
| GOOGLE_CLIENT_ID     | 구글 로그인을 위한 client_id     |
| GOOGLE_CLIENT_SECRET | 구글 로그인을 위한 client_secret |
| KAKAO_CLIENT_ID      | 카카오 로그인을 위한 client_id    |
| KAKAO_CLIENT_SECRET  | 카카오 로그인을 위한 client_secret|

