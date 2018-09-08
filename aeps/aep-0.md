# AEP 0 (structure and process of Adrian Enhancement Proposal)

## Current situation
Food for thought and decisions are not documented anywhere.

## Desired situation
1. Features are proposed as AEPs (with pros and cons listed, if applicable).
2. Adrian developers discuss AEPs and reach consensus.
3. Decisions, changes to AEPs are documented as part of implementation log.

## Numbering
Each AEP has number (positive integer). Numbers ≤ 100 are reserved for future use by
Adrian developers. Any number not currently in use by AEPs (including proposed AEPs)
is acceptable, contributors are encouraged to use max(largest used number, 100) + 1.
Number of rejected AEP may be reused.

## Structure
AEP has three mandatory sections:

* “Current situation” (current state of affairs)
* “Desired situation” (what AEP is trying to achieve)
* “Log” (implementation log)

In addition to mandatory sections, AEP may have extra content (before “Log” section),
optionally split into sections.

## Implementation log
“Log” section contains implementation log for AEP. Each log record contains
date (Y-m-d) and status.

Statuses:

* Proposed (is added by AEP author)
* Accepted (developers agreed to discuss AEP and maybe implement it)
* Implemented (AEP is fully implemented)
* Frozen (AEP is not implemented)

## Process
1. AEP is proposed via regular contribution process.
2. Adrian developers review AEP and decide to reject or accept it.
3. If AEP is accepted, AEP maintainer accepts contribution and adds log record
   with Accepted status.
4. When AEP is fully implemented, AEP maintainer adds log record with Implemented
   status.

Accepted AEP may become Frozen at any stage.

## Commit message (for maintainer)
* Set AEP-... to Accepted
* Set AEP-... to Frozen
* Set AEP-... to Implemented

Example: “Set AEP-0 to Accepted”.

When merging branches into master, use `--no-ff` and add “(AEP-...)” after regular
message.

Example: “Merge branch 'my-mega-feature' into master (AEP-0)”.

## Commit message (for AEP authors)
Propose AEP-...

Example: “Propose AEP-0”

## Log
* 2018-09-08. Proposed.
* 2018-09-08. Accepted.
