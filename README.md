# Software Design Description \<Component-SubComponent-Function-cProject Title\>

Information Classification: SAP Internal

**Table of Contents**

- [Project Information and Document Status](#project-information-and-document-status)
  - [Document Revision History](#document-revision-history)
  - [Document Template Information](#document-template-information)
- [General Information](#general-information)
  - [Stakeholders and Roles](#stakeholders-and-roles)
  - [References](#references)
- [Design](#design)
  - [Key Requirements and Design Goals](#key-requirements-and-design-goals)
  - [Context](#context)
  - [Major Building Blocks](#major-building-blocks)
  - [Interfaces and Communication Handling](#interfaces-and-communication-handling)
  - [Design Challenges](#design-challenges)
  - [User Interface / Ux Design](#user-interface--ux-design)
  - [Used Components and Frameworks](#used-components-and-frameworks)
  - [Package/Development Component Concept](#packagedevelopment-component-concept)
  - [Feature Flags / Switches and Dependencies](#feature-flags--switches-and-dependencies)
  - [Upgrade, Migration, Compatibility](#upgrade-migration-compatibility)
  - [TCO Considerations](#tco-considerations)
  - [Related Content](#related-content)
    - [Analytics and BI Content](#analytics-and-bi-content)
    - [API Hub](#api-hub)
  - [Compliance to Standards and Guidelines](#compliance-to-standards-and-guidelines)
    - [Applied Architecture and Design Guidelines](#applied-architecture-and-design-guidelines)
    - [Deviations](#deviations)
- [Design Details Documentation](#design-details-documentation)
  - [Business Objects / Database Design](#business-objects--database-design)
  - [Testability and Test Environment](#testability-and-test-environment)
  - [Complex Algorithms and Applied Patterns](#complex-algorithms-and-applied-patterns)
  - [Design Alternatives and Trade Offs](#design-alternatives-and-trade-offs)
  - [Guide to the Implementation](#guide-to-the-implementation)
- [Appendix](#appendix)
  - [Glossary](#glossary)
  - [Customizing](#customizing)
  - [Supportability Considerations](#supportability-considerations)
  - [Error Analysis](#error-analysis)
    - [Debugging](#debugging)
    - [Logging and Tracing](#logging-and-tracing)
    - [Other Error Analysis Tools](#other-error-analysis-tools)
  - [Other](#other)

> _**How to use this template**_
>
> _The purpose of a design doc is_
>
> 1. _develop design concepts for the target component,_
> 1. _enable review of the key concepts,_
> 1. _support alignment between stakeholders,_
> 1. _record trade-offs and reasons for major design decisions as needed._
> 1. _In addition we use this document as a 'Guide to the code' when the implementation is done._
>
> _Consider these guidelines when using this template:_
>
> - _Document only what is needed by reviewers and required for development of high quality software_
> - _Less is better! Write short / concise and so that it is easy to understand. Focus on key design challenges, not completeness (large documents are a waste and no one will read them!)_
> - _Write for ease of understanding but don't waste time on unnecessary polishing._
> - _'Pair Design' recommended (= develop key concepts together with a colleague)_
> - _Document design details & 'guide to the code' after the implementation. Only write what is not contained in the code/system already_
> - _A template is a template and not a form. Only fill what is needed and relevant._
> - _This template is a base template. We expect large development units to derive their own versions that are specific to their environment / technology._
>
> _Some guidelines on reviews_
>
> - _An Expert Review is mandatory only for chapter [Design](#design)_
> - _Differentiate between expert review (with few experts, deep review, issues & changes likely) and light review / information roll out (many people, no/few issues expected)._
> - _Expert Review Participants: only experts & directly affected people (provider and consumers)_
> - _Design for team topic: review with development team + stakeholders_
> - _(Public) Interfaces / Cross-alignment: Providers and consumers_
> - _Do a real meeting (not just mail / doc) to discuss the key design topics_
> - _Try for no more than 7 people!_
> - _Light Review Participants: There can be many participants. Can be mail / doc based, meetings to discuss remaining issues are optional_

## Project Information and Document Status

| . | . |
| -- | -- |
| Project Architect | n/n |
| Authors | n/n |

### Document Revision History

| Version | Date | Author | Remarks |
| ------- | ---- | ------ | ------- |
| draft | date | n/n | ... |

### Document Template Information

The original version of this template can be found [here in github.wdf.sap.corp](https://github.wdf.sap.corp/DevProcess-Architecture/SoftwareDesignDescription/blob/master/T011_SoftwareDesignDescription-Template.md).

The accompanying "Tips and Tricks" can be found [here](https://github.wdf.sap.corp/DevProcess-Architecture/SoftwareDesignDescription/blob/master/T011_SoftwareDesignDescription-Tips_and_Tricks.md).

## General Information

### Stakeholders and Roles

> List all people who were involved in the creation of this document and who should be involved in the final review and sign off of this document.

| Role | Name |
| ---- | ---- |
| Author(s) | |
| Architect | |
| Product Owner| |
| Information Developer | |
| Quality Responsible | |
| Ux Representative | |
| UA Representative | |
| Support Representative | |
| _other relevant roles_ | |

### References

> In this table give a list of all documents that are input / assigned to the design scope.

| Document Type | Document Title + Version | Link |
| ------------- | ------------------------ | ---- |
| Sirius Project | _Name_ | _Link_ |
| Architecture Design Document(s) | _Name_ | _Link_ |
| Requirements Specification | _Name_ | _Link_ |
| UI / Ux Design Specification | _Name_ | _Link_ |
| Backlog | _Name_ | _Link_ |
| SAP Patents based on this work | _Name_ | _Link_ |
| ... | | |

## Design

> Give a brief overview of the document / feature design, i.e. of the rest of the document. A reader / reviewer should be able to decide from this
> overview if the need to read the rest.

### Key Requirements and Design Goals

> Briefly describe the key requirements and design goals that drive this design. Consider the requirements from the release backlog but also **quality
> attributes and non-functional requirements** (they should all be in the backlog as well|. The reference to the backlog / requirements is given in the
> [References](#references) above.

### Context

> State the business context and technical context of the component, sub-components or function designed in this document. What is its purpose? Refer to requirements defined in the related _Software Requirements Specification_ to make clear, how this design fulfills them.
>
> Explain how the components and functions which are designed in this document fit into the overall architecture (as described in the corresponding ACD) and how they fulfill functional requirements and quality attribute scenarios (as described in the _Software Requirements Specification_). To do so, provide a TAM block diagram which shows the main building blocks of your component together with surrounding components, with which it is integrated.
>
> Note: the design has to comply with the boundary conditions given in the corresponding Architecture Concept Document.

### Major Building Blocks

> Deliver a holistic, easy to understand description of the design topic. Take and _outside view_ and adjust the zoom to show the high-level structure.
>
> Describe how the design topic is structured into major building blocks, which responsibilities each building block has and how they relate to each other. Typical building blocks on this level are packages or (sub-)components and important classes and interfaces.
>
> It is good practice to first focus on the most long-lived structures, which is typically the data / entity / business objects level (i.e. show the data model or the changes to the existing data model) and then describe the use cases / functions that use / traverse these objects.
>
> Use [TAM block diagrams](https://wiki.wdf.sap.corp/wiki/display/Modeling) and / or class diagrams to depict the building blocks and their relations.
> Additionally describe their responsibilities in the text.
>
> If helpful, visualize the described behavior of the system and the interaction between the major building blocks as sequence diagrams or activity
> diagrams (_who is talking to whom in which order?_).
>
> Describe typical interactions of client components with your component as sequence or activity diagrams and accompanying prose.
>
> If helpful include business object models or entity-relationship diagrams of the most important database tables (table names, important fields only, no data types). A more detailed database design can be included in section [Database Design](#database-design).
>
> After reading this section your stakeholders must be able to understand
>
> - the scope and the problem described in the document,
> - if the described solutions appropriately solves the problem,
> - which parts of the component must be kept flexible and variable,
> - the core principles of the document,
> - potential future restrictions for extending the component.
>
> Designers of client components should find all they need in this section.
>
> - Use sequence diagrams for simple behavior, activity diagram for complex behavior.
> - Restrict entity-relationship diagrams here to the most important database tables and their relationships, depict only the most important fields, omit types.

### Interfaces and Communication Handling

> Special considerations should be given to APIs for the designed functionality. A major customer-public API is worth designing in a separate design.
> **Do not make incompatible changes to existing APIs** without approval from the program architect.
>
> Always include the potential clients / users of the API in the initial design discussions. It is a good practice to also define code examples, in the best case as test code, for any public API.
>
> The interface description has to include the following information:
>
> - Type of interface: OData. API, BAPI, RFC, IDoc, Report-Report-Interface, ...
> - Interface structure of reference to separate document(s)
> - which generic parts (e.g. generated structures or names-value containers) does the interface contain?
> - Show usage examples of the interfaces that show key arguments, sequence of calls etc.

### Design Challenges

> Quality attributes / non-functional requirements have a major influence on design decisions and must be considered early on. Describe briefly the non-functional requirements / Quality Attribute Scenarios (e.g. from higher level designs / ACD) that have to be satisfied with this design. Add a reference to detailed descriptions / the backlog if this is needed / useful. Describe how these design challenges were solved and how the solutions was verified (e.g. by a prototype, ...).
>
> Example: _To achieve the goal of processing 100 million records per hour, parallelization is used. The prototype on the performance test system achieved already 85 million records per hour. Goal can be achieved with XXX SAPS._

### User Interface / Ux Design

> Ux design is very important for usability and the end-user perception of your software. Reviewing the UI of a feature is just as important as architecture / design topics. For major development / changes in UI, consider having a professional Ux designer do or at least review the UI parts.
> Provide at least use cases and low fidelity mock-ups for the user interactions (transactions, pop-ups, configuration screens, ...) e.g. with Powerpoint or the like. Focus on planned interaction steps, layout, texts, ... that will be visible to end users.

### Used Components and Frameworks

> State which components and frameworks are re-used and give the reason. If possible indicate the components and frameworks in the block / component diagram of chapter [Key Requirements and Design Goals](#key-requirements-and-design-goals) or include a separate one. Technology decisions have to be documents in ACD.
>
> Consider whether software components can be re-used. Refer to the corresponding chapter _Reuse of Existing Components_ in the related in the related ACD and Requirement Specification documents.

| Name | Description | Software Components | Implications |
| ---- | ----------- | ------------------- | ------------ |
| _enter name of the re-used component_ | _provide a detailed description of the functionality used of the re-used component_ | _specify the software component of the re-used / dependent component (e.g. SAP_ABAP, ENGFACADE)_ | _describe the impact for configuration, data, shipment, etc._ |
| | | | |

### Package/Development Component Concept

> Refine the package concept of the ACD for the part of the application this design documents focuses on:
>
> - Which packages / software components / development components does it consist of?
> - What is the relation of these entities to the overall structure?
> - How are these structured internally?
>
> Use package diagrams to show the usage dependencies. If necessary/applicable, distinguish between design time, configuration time and runtime dependencies. Describe which package interface is involved in which usage dependency. If you use a general pattern (e.g. each package has an interface with identical name), describe only the pattern and omit a repetitive list of package interfaces.

### Feature Flags / Switches and Dependencies

> This section is relevant if new development should not affect existing functionality.

### Upgrade, Migration, Compatibility

> If the present software design does not deal with a completely new functionality, it is usually based on an existing design that is enhanced, adjusted or corrected. _Migration_ should describe how any kind of content of the previous implementation can be moved into the present design without feature loss.
>
> In any refinement case provide in this chapter a comparison how the current design relates to an existing implementation of a previous design, that is code, database content in either customer data or configuration case. Also reflect how the new solution may be deployed on an existing implementation.
> For this purpose the **compatibility and interoperability rules** of the architecture guideline must be recognized. Consider dependencies within the previous implementation that might imply further activities. Also rules and recommendations of the TICM product standard must be considered. From the total cost of ownership (TCO) perspective **XPRAs and manual upgrade / migration steps must be avoided**, but if inevitably required shall be described here as well.

### TCO Considerations

> TCO (total cost of ownership) is a major consideration for customers. _SAP is too expensive to set up, configure use operationally_ is often stated by customers. Some of these aspects (technology selection, systems / servers, etc.) are already handled at the ACD level.
>
> Describe how low TCO is achieved by your design for these and other relevant aspects on the design level: these aspects - administration / config complexity, need for monitoring, dependency on specific version of other components - and finally end-user usability.

### Related Content

#### Analytics and BI Content

> If you create new data or change existing data (entities, DB tables, ...) consider if this data / change must be reflected in central BI reporting / content, check if the BI reporting for this data is defined as a requirement in the backlog.
>
> For BI content development itself, use the template provided on the respective [wiki page](https://wiki.wdf.sap.corp/display/BICCC/Specification+and+Design) into this chapter.

#### API Hub

### Compliance to Standards and Guidelines

> Consider Product Standard requirements, industry standard requirements, as well as guidelines which are relevant for this design.

#### Applied Architecture and Design Guidelines

> Maintain version and storage location of the architecture guidelines, development guidelines and programming guidelines relevant for this project. In case of deviation from guidelines, document it here (guideline, from which design deviates; reason for deviation, approved by n/n).

| Guideline Name | Version / Date | Link |
| -------------- | -------------- | ---- |
| _tbd_ | _tbd_ | _tbd_ |

#### Deviations

> Please document here any deviations from product standards or architecture for this design. These can be deviations that you already know from the program / architecture level (that apply to you) or ones that surfaced during development.
>
> In case of new deviations arising development, you must contact your program architect.
>
> Note down if these deviations are temporary or long-term.

## Design Details Documentation

> This section is not part of the review but part of the developers / teams documentation of relevant details. **Write only what is needed to understand non-obvious details and what is not documented / contained in the code / system itself**.

### Business Objects / Database Design

> **Data Views** describe the database layout including: tables, additional fields and relations between tables. Consider the object-relational mapping between classes / objects and the database tables. Relevance: needed when database tables are created or extended. Use entity-relationship diagram (UML class diagrams using attribute stereotypes to mark key fields). Distinguish between system, configuration and application tables, e.g. by using stereotypes for the delivery class.
>
> **Archiving:** Do not forget archiving. Details can be found in the [Data Management cloud principle](https://wiki.wdf.sap.corp/wiki/pages/viewpage.action?pageId=1850354480).

### Testability and Test Environment

> Briefly document the design elements that support isolated testability of this component / feature (e.g. mocking interfaces, test approach).
>
> From a development perspective describe what should be tested and how (manual vs. automated, which unit test framework is used, which mock object framework or generator or which capture-replay mechanism).
>
> If there are special requirements for test execution, describe the test environment that is required to run the tests:
>
> - Which system landscape is necessary?
> - Which customizing settings (standard customizing, business content) are prerequisites?
> - Which application data (master data and transaction data) is required? How is it generated?
>
> **Note:** all implemented unit tests should be self-contained. That means unit tests should not rely on any external context e.g. certain customizing or master data. Standard delivery customizing or business content can be used. If any external data or settings are needed they should be created in the test class itself or faked via mock objects.

### Complex Algorithms and Applied Patterns

> If the design involves complex logic / algorithms, sketch the algorithm you are using. Make clear which parts of your development are driven by used frameworks or APIs and which parts really need new concepts which must be described here.
>
> Try to structure your algorithms according to software patterns (see <https://wiki.wdf.sap.corp/display/SoftwarePatterns> and <http://en.wikipedia.org/wiki/Software_design_pattern>). Explain why you think that this pattern matches the requirement, consider the aspects of possible future extensions to your code, supportability and simplicity.

### Design Alternatives and Trade Offs

> If there were significant design alternatives and trade-offs, describe them briefly here. The purpose is to later remember why a certain design decision was made. When a decision between two alternatives was not trivial, it is usually worth documenting.
>
> It is a good practice to document design alternatives as follows:
>
> - Definition of the trade-off / topic
> - What are the competing _forces_ (that create the design tension / trade-off)
> - Alternatives
> - Chose alternative with reason

### Guide to the Implementation

> This section should be written after the implementation is completed. Briefly describe _where to find what_ in the code, DDIC or other tools / repositories that are relevant.
>
> This information is very useful for yourself after a few month but also for handovers of code responsibility.

## Appendix

### Glossary

> In case you introduce new terms or terms unknown to an average SAP developer, use this chapter to list and explain them in a short and concise way.

| Term | Abbreviation | Definition |
| ---- | ------------ | ---------- |
| _tbd_ | _tbd_ | _tbd_ |

### Customizing

> List new and existing customizing which is important to run the developed functionality. What is delivered by SAP? What has to be setup by customers?
> Consider ABAP and non-ABAP parts.
>
> If several technical components are involved, e.g. ERP and CRM: Is it necessary to sync customizing? How is it done?
>
> This chapter should enable colleagues to set up the developed functionality in a test or demo system. Therefore try to cover all customizing aspects and also provide screen shots if necessary.

### Supportability Considerations

> Consider that the design documents will serve as technical and handover documentation, so have the needs of Support in mind: Describe known restrictions regarding restricted availability of functions in certain environments.
>
> List important customer exists and describe how they can be used.
>
> Describe known pitfalls, e.g. caused by inconsistent customizing which cannot be handled properly. Situations like this might appear during MIT. List important configuration documentation which helps to configure the system properly.
>
> If possible, describe how inconsistent data can be identified and corrected (see also [Other Error Analysis Tools](#other-error-analysis-tools)).

### Error Analysis

#### Debugging

> Enter tips & tricks for debugging.
>
> Where to start debugging? Where to put breakpoints when a problem arises? If the scenario involves processes which are spread over different components (ABAP -> ABAP, J2EE -> ABAP for example): How is it possible to debug the whole process? Link the technical documentation.
>
> If ABAP breakpoints are shipped: List check point groups from [Transaction SAAB](https://help.sap.com/saphelp_snc700_ehp01/helpdata/en/49/175f0da2ec14d2e10000000a42189b/content.htm) to activate breakpoints and the location of the breakpoints.
>
> What can be manipulated (_repped_) during debugging without risk? Where do I have to exit the debugging without leaving persistent changes on the database?

#### Logging and Tracing

> Especially for non-ABAP environments where debugging is sometimes difficult: Is it possible to use tracing to solve problems? Give keywords to search for in trace files, give advice on trace level and on the source of different trace files if applicable.
>
> In ABAP environments: Which tool is used for logging? For application log, give hints on how to view the results and how to interpret it. In case you are using log points, enter the checkpoint group from [Transaction SAAB](https://help.sap.com/saphelp_snc700_ehp01/helpdata/en/49/175f0da2ec14d2e10000000a42189b/content.htm) and describe how to use the logging results.
>
> If you foresee a central place where problems can occur, e.g. the call of an API to a different system, elaborate on tracing and error handling. How is a certain error situation reflected in the trace output?

#### Other Error Analysis Tools

> If existing: describe analysis and corrections reports or transactions. Are there test environments which can facilitate the tracking of errors?
> Example: The application consists of a Java Web Application which talks to an ABAP back-end. A problem occurs in the ABAP back-end. A tool exists which allows for persisting the data which is sent from the Java on ABAP side, it is possible to track the problem with pure ABAP means.
>
> State relevant support notes that might be useful in that context.

### Other

> Important information that does not fit into one of the preceding sections...
