# 역할과 책임
> domain 모듈은 특정 하나의 도메인에 대한 책임만 가집니다.

# 실행방법(사용방법)
**사용하고자 하는 모듈의 dependencies 블록 내에 다음과 같이 추가해야 합니다.**
```
implementation project(":domain:xxx")
api project(":domain:xxx")
```

# 관례
```
모듈 명 : xxx-domain
패키지 명 : com.uket.domain.xxx
```
