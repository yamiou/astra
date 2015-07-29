.. SenseKit documentation master file, created by
   sphinx-quickstart on Tue Jul 14 22:36:35 2015.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. |sdkname| replace:: Astra
.. |numberofsections| replace:: three

The |sdkname| SDK
====================================

Welcome to the book on the |sdkname| SDK! The |sdkname| SDK's exposes to the developer a comprehensive set of functionality to utilize all of the Astra's capabilities through a simple yet powerful API.

This book will be a vital resource to any developer who wants to:

- Quickly get up to speed with |sdkname| SDK development
- Understand the high-level concepts with the SDK
- Find detailed information on specific SDK functionality or features

"The |sdkname| SDK" begins below with an introduction, followed by these |numberofsections| sections:

.. toctree::
   :titlesonly:
   :maxdepth: 1

   gettingstarted
   concepts
   retrievingstreamdata


Once you've read the introduction, jump over to the :doc:`Getting Started <gettingstarted>` section to explore an introductory project.

Introduction
============

The design goals for the SDK are straightforward and lofty - to create the world's best 3D sensor development experience. To facilitate this, the best ideas from many other modern device and sensor SDKs, along with many original ideas from Orbbec's development team, were combined to put emphasis on developer creativity, remove needless complexity, and generally make |sdkname| SDK development a joyful, unintrusive process.

Low Ceremony Design
===================

- API consistency
- 80% of commonly used functionality is put to the front, along with the remainder

The |sdkname| SDK reduces the boilerplate code required to obtain sensor data by exposing high-level stream types in addition to low-level stream types.

Cross-Everything
================

The |sdkname| SDK exposes its functionality through a core C API and also provides a modern C++11 API. From this foundation, the SDK is able to support a number of languages and platforms:

Currently Supported
-------------------

- Languages: C, C++11, Java
- Platforms: Windows, Android

Planned Support
---------------

- Languages: C#, Unity
- Platforms: Linux, OSX

Mobile
======

Mobile devices are becoming more powerful and smaller each passing year, but are comparatively resource contrained to desktop PCs. With this in mind, the |sdkname| SDK has been carefully engineered to minimize CPU and memory overhead to keep mobile devices running smoothly while economizing battery usage.

Extensibility
=============

- plugins

Perhaps one of its more exciting features, the |sdkname| SDK can support a full network of sensors. Additionally, even if different sensors provide different types of streams, |sdkname| can use data flowing from different sensors to compose higher-level stream types.

:doc:`2 Getting Started <gettingstarted>`

.. Indices and tables
   ==================
   * :ref:`genindex`
   * :ref:`modindex`
   * :ref:`search`